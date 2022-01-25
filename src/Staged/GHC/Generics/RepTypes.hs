{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- For default Rep definition
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK --not-home #-}
module Staged.GHC.Generics.RepTypes (
    -- * Generic representation types
    V2,
    U2 (..),
    M2 (..),
    K2 (..),
    Par2 (..),
    (:++:) (..),
    (:**:) (..),
    (:@@:) (..),
    -- * Synonyms for convenience
    D2, C2, S2,
    D, C, S, R,
    -- * Meta-information
    Datatype (..),
    Constructor (..),
    Selector (..),
    Fixity (..),
    FixityI (..),
    Associativity (..),
    SourceUnpackedness (..),
    SourceStrictness (..),
    DecidedStrictness (..),
    Meta (..),
    -- * Rep translation
    Translate,
    -- * TH Types
    Code, Quote
) where

import Data.Kind           (Type)
import GHC.Generics
       (Associativity (..), C, Constructor (..), D, Datatype (..),
       DecidedStrictness (..), Fixity (..), FixityI (..), Meta (..), R, S,
       Selector (..), SourceStrictness (..), SourceUnpackedness (..))
import qualified GHC.Generics as GHC
import Language.Haskell.TH (Code, Quote)
import GHC.TypeLits        (ErrorMessage (..), TypeError)
import qualified Staged.GHC.Generics.FakeGeneric1 as Fake

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

data V2 (q :: Type -> Type) (p :: k)

deriving instance Eq (V2 q p)
deriving instance Ord (V2 q p)
deriving instance Show (V2 q p)
deriving instance Read (V2 q p)
deriving instance GHC.Generic (V2 q p)
instance Fake.Generic1 (V2 q)

data U2 (q :: Type -> Type) (p :: k) = U2

deriving instance Eq (U2 q p)
deriving instance Ord (U2 q p)
deriving instance Show (U2 q p)
deriving instance Read (U2 q p)
deriving instance GHC.Generic (U2 q p)
instance Fake.Generic1 (U2 q)

newtype M2 (i :: Type) (c :: Meta) (f :: (Type -> Type) -> k -> Type) (q :: Type -> Type) (p :: k)
    = M2 { unM2 :: f q p }

deriving instance Eq (f q p) => Eq (M2 i c f q p)
deriving instance Ord (f q p) => Ord (M2 i c f q p)
deriving instance Show (f q p) => Show (M2 i c f q p)
deriving instance Read (f q p) => Read (M2 i c f q p)
deriving instance GHC.Generic (M2 i c f q p)
instance Fake.Generic1 (M2 i c f q)

newtype K2 c (q :: Type -> Type) (p :: k)
    = K2 { unK2 :: q c }

deriving instance Eq (q c) => Eq (K2 c q p)
deriving instance Ord (q c) => Ord (K2 c q p)
deriving instance Show (q c) => Show (K2 c q p)
deriving instance Read (q c) => Read (K2 c q p)
deriving instance GHC.Generic (K2 c q p)
instance Fake.Generic1 (K2 c q)

infixr 5 :++:
data ((f :: (Type -> Type) -> k -> Type) :++: (g :: (Type -> Type) -> k -> Type)) (q :: Type -> Type) (p :: k)
    = L2 (f q p)
    | R2 (g q p)

deriving instance (Eq (f q p), Eq (g q p)) => Eq ((f :++: g) q p)
deriving instance (Ord (f q p), Ord (g q p)) => Ord ((f :++: g) q p)
deriving instance (Show (f q p), Show (g q p)) => Show ((f :++: g) q p)
deriving instance (Read (f q p), Read (g q p)) => Read ((f :++: g) q p)
deriving instance GHC.Generic ((f :++: g) q p)
instance Fake.Generic1 ((f :++: g) q)

infixr 6 :**:
data ((f :: (Type -> Type) -> k -> Type) :**: (g :: (Type -> Type) -> k -> Type)) (q :: Type -> Type) (p :: k)
    = f q p :**: g q p

deriving instance (Eq (f q p), Eq (g q p)) => Eq ((f :**: g) q p)
deriving instance (Ord (f q p), Ord (g q p)) => Ord ((f :**: g) q p)
deriving instance (Show (f q p), Show (g q p)) => Show ((f :**: g) q p)
deriving instance (Read (f q p), Read (g q p)) => Read ((f :**: g) q p)
deriving instance GHC.Generic ((f :**: g) q p)
instance Fake.Generic1 ((f :**: g) q)

-- https://gitlab.haskell.org/ghc/ghc/-/issues/15969#note_164233 !!!
infixl 7 :@@:
newtype ((f :: (Type -> Type) -> k2 -> Type) :@@: (g :: k1 -> k2)) (q :: Type -> Type) (p :: k1)
    = App2 { unApp2 :: f q (g p) }

deriving instance Eq (f q (g p)) => Eq ((f :@@: g) q p)
deriving instance Ord (f q (g p)) => Ord ((f :@@: g) q p)
deriving instance Show (f q (g p)) => Show ((f :@@: g) q p)
deriving instance Read (f q (g p)) => Read ((f :@@: g) q p)
deriving instance GHC.Generic ((f :@@: g) q p)
instance Fake.Generic1 ((f :@@: g) q)

newtype Par2 (q :: k -> Type) (p :: k)
    = Par2 { unPar2 :: q p }

deriving instance Eq (q p) => Eq (Par2 q p)
deriving instance Ord (q p) => Ord (Par2 q p)
deriving instance Show (q p) => Show (Par2 q p)
deriving instance Read (q p) => Read (Par2 q p)
deriving instance GHC.Generic (Par2 q p)
instance Fake.Generic1 (Par2 q)

-------------------------------------------------------------------------------
-- Synonyms for convenience
-------------------------------------------------------------------------------

type D2 = M2 D
type C2 = M2 C
type S2 = M2 S

-- | Translate "GHC.Generics" 'GHC.Rep' type into our 'Rep' type.
type family Translate (f :: k -> Type) :: (Type -> Type) -> k -> Type where
    Translate (GHC.M1 i c f) = M2 i c (Translate f)
    Translate (GHC.K1 R c)   = K2 c
    Translate (f GHC.:+: g)  = Translate f :++: Translate g
    Translate (f GHC.:*: g)  = Translate f :**: Translate g
    Translate (GHC.Rec1 f)   = Par2 :@@: f
    Translate GHC.Par1       = Par2
    Translate GHC.U1         = U2
    Translate GHC.V1         = V2
    Translate (f Fake.:.: g)  = Translate f :@@: g
    Translate x               = TypeError ('Text "Translate error: " ':<>: 'ShowType x)
