{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes    #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Staged.GHC.Generics.GHCish
  ( GGeneric (..)
  , ghcGenericTo
  , ghcGenericFrom
  ) where

import Data.Functor.Const         (Const (..))
import Data.Kind                  (Constraint, Type)
import Language.Haskell.TH.Lib    (caseE, conE, conP, match, normalB, varE, varP)
import Language.Haskell.TH.Syntax (Exp, Match, Name, mkNameG_d, newName, unTypeCode, unsafeCodeCoerce)

import qualified GHC.Generics as GHC

import Staged.GHC.Generics.RepTypes

-- A proxy type for GHC.Generics functions
data Prox (d :: Meta) (f :: j -> Type) (a :: j) = Prox
toProx :: forall j (q :: Type -> Type) i (d :: Meta) (f :: (Type -> Type) -> j -> Type) (a :: j).
  M2 i d f q a -> Prox d (f q) a
toProx _ = Prox

-- | Use GHC generics to implement the 'to' method.
ghcGenericTo :: (GHC.Generic a, GGeneric (Translate (GHC.Rep a)), Quote q)
  => Translate (GHC.Rep a) (Code q) x -> Code q a
ghcGenericTo = unsafeCodeCoerce . gto

-- | Use GHC generics to implement the 'from' method.
ghcGenericFrom :: (GHC.Generic a, GGeneric (Translate (GHC.Rep a)), Quote q)
  => Code q a -> (Translate (GHC.Rep a) (Code q) x -> Code q r) -> Code q r
ghcGenericFrom c k = unsafeCodeCoerce $ caseE (unTypeCode c) $ gmatches k

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
    makeNames :: Quote q => Int -> q (f (Const Name) x, [Name] -> [Name], Int)

instance (GMakeNames f, GMakeNames g) => GMakeNames (f :**: g) where
    makeNames n = do
        (l, l', m) <- makeNames n
        (r, r', p) <- makeNames m
        return (l :**: r, l' . r', p)

instance GMakeNames U2 where
    makeNames n = return (U2, id, n)

instance GMakeNames f => GMakeNames (M2 i c f) where
    makeNames n = do
        (x, x', m) <- makeNames n
        return (M2 x, x', m)

instance GMakeNames (K2 c) where
    makeNames n = do
        name <- newName ("v" ++ show n)
        return (K2 (Const name), (name:), n + 1)

instance (Constructor c, GGenericFields f, GMakeNames f) => GGenericCon (C2 c f) where
  gtoCon namer m@(M2 fqp) = gtoFields (conE conN) fqp
    where
      conN :: Name
      conN = namer (conName (toProx m))
  gmatchesCon namer k rest = (do
    (names, namesb', _) <- makeNames 0
    let names' = namesb' []
    match (conP conN (map varP names')) (normalB . unTypeCode . k . M2 $ grebuild names) []
    ) : rest
    where
      conN :: Name
      conN = namer (conName (Prox @_ @c))

class GGenericFields f where
  gtoFields :: Quote q => q Exp -> f (Code q) x -> q Exp
  grebuild :: Quote q => f (Const Name) x -> f (Code q) x

instance GGenericFields f => GGenericFields (S2 c f) where
  gtoFields c (M2 x) = gtoFields c x
  grebuild = M2 . grebuild . unM2

instance GGenericFields U2 where
  gtoFields c U2 = c
  grebuild U2 = U2

instance (GGenericFields f, GGenericFields g) => GGenericFields (f :**: g) where
  gtoFields c (x :**: y) = gtoFields (gtoFields c x) y
  grebuild (l :**: r) = grebuild l :**: grebuild r

instance GGenericFields (K2 c) where
  gtoFields c (K2 x) = [| $c $(unTypeCode x) |]
  grebuild (K2 (Const a)) = K2 (unsafeCodeCoerce (varE a))
