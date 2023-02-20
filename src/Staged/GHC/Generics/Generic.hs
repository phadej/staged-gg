{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes    #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

-- | This module offers facilities for producing 'Staged.GHC.Generics.Generic'
-- representations and methods based on the corresponding
-- @"GHC.Generic".'GHC.Generics.Generic'@ ones.
module Staged.GHC.Generics.Generic
  ( GGeneric
  , GRep
  , genericTo
  , genericFrom
  ) where

import Control.Applicative        (liftA2)
import Data.Functor.Const         (Const (..))
import Data.Semigroup             (Endo (..), Dual (..))
import Data.Kind                  (Constraint, Type)
import Language.Haskell.TH.Lib    (caseE, conE, conP, match, normalB, varE, varP, appE)
import Language.Haskell.TH.Syntax (Exp, Match, Name, mkNameG_d, newName, unTypeCode, unsafeCodeCoerce)
import Data.Functor.Identity      (Identity (..))

import qualified GHC.Generics as GHC

import Staged.GHC.Generics.RepTypes

-- A proxy type for GHC.Generics functions
data Prox (d :: Meta) (f :: j -> Type) (a :: j) = Prox
toProx :: forall j (q :: Type -> Type) i (d :: Meta) (f :: (Type -> Type) -> j -> Type) (a :: j).
  M2 i d f q a -> Prox d (f q) a
toProx _ = Prox

-- | Use GHC generics to implement the 'Staged.GHC.Generics.Rep'
-- associated type.
type GRep a = Translate (GHC.Rep a)

-- | Use GHC generics to implement the 'Staged.GHC.Generics.to' method.
genericTo :: (GHC.Generic a, GGeneric (Translate (GHC.Rep a)), Quote q)
  => GRep a (Code q) x -> Code q a
genericTo = unsafeCodeCoerce . gto

-- | Use GHC generics to implement the 'Staged.GHC.Generics.from' method.
genericFrom :: (GHC.Generic a, GGeneric (Translate (GHC.Rep a)), Quote q)
  => Code q a -> (GRep a (Code q) x -> Code q r) -> Code q r
genericFrom c k = unsafeCodeCoerce $ caseE (unTypeCode c) $ gmatches k

{-
-- `from` is not the most obvious thing, in general. I found it quite
-- helpful to look at an example of what we're trying to build.

data MyType a = My1 a | My2 a a
  deriving (G.Generic)

instance Generic (MyType a) where
  from x k = unsafeCodeCoerce $ caseE (unTypeCode x) [
      do
        v1 <- newName "v1"
        match (conP 'My1 [varP v1])
              (normalB $ unTypeCode $ k (M2 (L2 (M2 (M2 (K2 (unsafeCodeCoerce $ varE v1)))))))
              []
    , do
        v1 <- newName "v1"
        v2 <- newName "v2"
        match (conP 'My2 [varP v1, varP v2])
              (normalB $ unTypeCode $ k (M2 (R2 (M2
                (M2 (K2 (unsafeCodeCoerce $ varE v1)) :**: M2 (K2 (unsafeCodeCoerce $ varE v2)))))))
              []
    ]
-}

-- | A class for generic representations of types supporting
-- staged generic operations.
class GGeneric (f :: (Type -> Type) -> Type -> Type) where
  gto :: Quote q => f (Code q) x -> q Exp

  -- Build the list of case branches.
  gmatches :: Quote q => (f (Code q) x -> Code q r) -> [q Match]

-- | Make a "namer" function holding on to a package and module name to
-- eventually make a Name out of the bare constructor name
mkNamer :: forall (d :: Meta). Datatype d => String -> Name
mkNamer = mkNameG_d (packageName prox) (moduleName prox)
  where
    prox :: Prox d z w
    prox = Prox

instance (Datatype d, GGenericCon f) => GGeneric (D2 d f) where
  gto (M2 fqp) = gtoCon (mkNamer @d) fqp
  gmatches k = gmatchesCon (mkNamer @d) (k . M2) []

class GGenericCon (f :: (Type -> Type) -> Type -> Type) where
  gtoCon :: Quote q => (String -> Name) -> f (Code q) x -> q Exp
  -- We take a "namer" function that holds on to the package and module names
  -- for us so we can apply the constructor name and get its 'Name'.
  gmatchesCon :: Quote q => (String -> Name) -> (f (Code q) x -> Code q r) -> [q Match] -> [q Match]

instance GGenericCon V2 where
  gtoCon _ x = case x of
  gmatchesCon _ _ [] = []
  gmatchesCon _ _ _ = error "V2 shouldn't lead to more constructors!"

instance (GGenericCon f, GGenericCon g) => GGenericCon (f :++: g) where
  gtoCon namer (L2 x) = gtoCon namer x
  gtoCon namer (R2 x) = gtoCon namer x

  gmatchesCon namer k = gmatchesCon @f namer (k . L2) . gmatchesCon @g namer (k . R2)

type GMakeNames :: forall {k}. ((Type -> Type) -> k -> Type) -> Constraint
class GMakeNames (f :: (Type -> Type) -> k -> Type) where
    makeNames :: Quote q => Int -> q (f (Const Name) x, Int)

instance (GMakeNames f, GMakeNames g) => GMakeNames (f :**: g) where
    makeNames n = do
        (l, m) <- makeNames n
        (r, p) <- makeNames m
        return (l :**: r, p)

instance GMakeNames U2 where
    makeNames n = return (U2, n)

instance GMakeNames f => GMakeNames (M2 i c f) where
    makeNames n = do
        (x, m) <- makeNames n
        return (M2 x, m)

instance GMakeNames (K2 c) where
    makeNames n = do
        name <- newName ("v" ++ show n)
        return (K2 (Const name), n + 1)

instance (Constructor c, GMakeNames f, GTraversey f) => GGenericCon (C2 c f) where
  gtoCon namer m@(M2 fqp) = gfoldyl (\f x -> f `appE` unTypeCode x) (conE conN) fqp
    where
      conN :: Name
      conN = namer (conName (toProx m))
  gmatchesCon namer k rest = (do
    (names, _) <- makeNames 0
    let names' = gfoldyr (\(Const n) r -> n : r) [] names
    match (conP conN (map varP names')) (normalB . unTypeCode . k . M2 $ grebuild names) []
    ) : rest
    where
      conN :: Name
      conN = namer (conName (Prox @_ @c))

      grebuild :: Quote q => f (Const Name) x -> f (Code q) x
      grebuild = gfmappy $ \(Const name) -> unsafeCodeCoerce (varE name)

-- | A sort of traversal. The implementation isn't very efficient for general
-- applicatives, but we're only using Const and Identity, where
-- fmap is incredibly cheap.
class GTraversey f where
  gtraversey :: forall m q s x. Applicative m => (forall z. q z -> m (s z)) -> f q x -> m (f s x)

gfoldyMap :: forall f m q x. (GTraversey f, Monoid m) => (forall z. q z -> m) -> f q x -> m
gfoldyMap f = getConst . gtraversey (Const . f)

gfmappy :: forall f q s x. GTraversey f => (forall z. q z -> s z) -> f q x -> f s x
gfmappy f = runIdentity . gtraversey (Identity . f)

instance GTraversey f => GTraversey (M2 i c f) where
  gtraversey f (M2 x) = M2 <$> gtraversey f x

instance (GTraversey f, GTraversey g) => GTraversey (f :++: g) where
  gtraversey f (L2 x) = L2 <$> gtraversey f x
  gtraversey f (R2 x) = R2 <$> gtraversey f x

instance (GTraversey f, GTraversey g) => GTraversey (f :**: g) where
  gtraversey f (x :**: y) = liftA2 (:**:) (gtraversey f x) (gtraversey f y)

instance GTraversey (K2 c) where
  gtraversey f (K2 c) = K2 <$> f c

instance GTraversey Par2 where
  gtraversey f (Par2 x) = Par2 <$> f x

instance GTraversey U2 where
  gtraversey _ U2 = pure U2

instance GTraversey V2 where
  gtraversey _ x = case x of

gfoldyr :: GTraversey f => (forall z. q z -> r -> r) -> r -> f q x -> r
gfoldyr f z t = appEndo (gfoldyMap (Endo . f) t) z

gfoldyl :: GTraversey f => (forall z. r -> q z -> r) -> r -> f q x -> r
gfoldyl f z t = appEndo (getDual (gfoldyMap (Dual . Endo . flip f) t)) z
