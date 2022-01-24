{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TemplateHaskellQuotes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Staged.GHC.Generics.GHCish
  ( ghcGenericTo
  , ghcGenericFrom
  ) where

import Staged.GHC.Generics.Types
import qualified GHC.Generics as G
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Lib
import Data.Kind (Type, Constraint)
import Data.Bifunctor (first)

-- A proxy type for GHC.Generics functions
data Prox (d :: Meta) (f :: j -> Type) (a :: j) = Prox
toProx :: forall j (q :: Type -> Type) i (d :: Meta) (f :: (Type -> Type) -> j -> Type) (a :: j).
  M2 i d f q a -> Prox d (f q) a
toProx _ = Prox

-- | Use GHC generics to implement the 'to' method.
ghcGenericTo :: (Rep a ~ Translate (G.Rep a), G.Generic a, GGeneric (Rep a), Quote q)
  => Rep a (Code q) x -> Code q a
ghcGenericTo = unsafeCodeCoerce . gto

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

-- | Use GHC generics to implement the 'from' method.
ghcGenericFrom :: (Rep a ~ Translate (G.Rep a), G.Generic a, GGeneric (Rep a), Quote q)
  => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
ghcGenericFrom c k = unsafeCodeCoerce $ caseE (unTypeCode c) $ gmatches k

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

type GNumFields :: forall {k}. ((Type -> Type) -> k -> Type) -> Constraint
class GNumFields (f :: (Type -> Type) -> k -> Type) where
  gnumFields :: Int
instance (GNumFields f, GNumFields g) => GNumFields (f :**: g) where
  gnumFields = gnumFields @f + gnumFields @g
instance GNumFields U2 where
  gnumFields = 0
instance GNumFields (S2 c f) where
  gnumFields = 1

instance (Constructor c, GGenericFields f, GNumFields f) => GGenericCon (C2 c f) where
  gtoCon namer m@(M2 fqp) = gtoFields (conE conN) fqp
    where
      conN :: Name
      conN = namer (conName (toProx m))
  gmatchesCon namer k rest = [do
    let name_bases = ["v" ++ show n | n <- [1..gnumFields @f]]
    names <- traverse newName name_bases
    match (conP conN (map varP names)) (normalB . unTypeCode . k . M2 $ fst (grebuild names)) []
    ] ++ rest
    where
      conN :: Name
      conN = namer (conName (Prox @_ @c))

class GGenericFields f where
  gtoFields :: Quote q => q Exp -> f (Code q) x -> q Exp
  grebuild :: Quote q => [Name] -> (f (Code q) x, [Name])

instance GGenericFields f => GGenericFields (S2 c f) where
  gtoFields c (M2 x) = gtoFields c x
  grebuild = first M2 . grebuild

instance GGenericFields U2 where
  gtoFields c U2 = c
  grebuild [] = (U2, [])
  grebuild _ = error "oopsy"

instance (GGenericFields f, GGenericFields g) => GGenericFields (f :**: g) where
  gtoFields c (x :**: y) = gtoFields (gtoFields c x) y
  grebuild v
    | (l, v') <- grebuild v
    , (r, v'') <- grebuild v'
    = (l :**: r, v'')

instance GGenericFields (K2 c) where
  gtoFields c (K2 x) = [| $c $(unTypeCode x) |]
  grebuild [] = error "Not enough names!"
  grebuild (a : as) = (K2 (unsafeCodeCoerce (varE a)), as)
