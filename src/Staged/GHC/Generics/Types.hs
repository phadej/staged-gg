{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- For default Rep definition
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK --not-home #-}
module Staged.GHC.Generics.Types (
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
    D, C, S,
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
    -- * Generic type classes
    Generic (..),
    Generic1 (..),
    -- * TH Types
    Code, Quote,
    -- * Utilities
    Translate,
) where

import Data.Kind           (Type)
import GHC.Generics
       (Associativity (..), C, Constructor (..), D, Datatype (..),
       DecidedStrictness (..), Fixity (..), FixityI (..), Meta (..), R, S,
       Selector (..), SourceStrictness (..), SourceUnpackedness (..))
import GHC.TypeLits        (ErrorMessage (..), TypeError)
import Language.Haskell.TH (Code, Quote)

import qualified GHC.Generics as GHC

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

data V2 (q :: Type -> Type) (p :: k)

deriving instance Eq (V2 q p)
deriving instance Ord (V2 q p)
deriving instance Show (V2 q p)
deriving instance Read (V2 q p)

data U2 (q :: Type -> Type) (p :: k) = U2

deriving instance Eq (U2 q p)
deriving instance Ord (U2 q p)
deriving instance Show (U2 q p)
deriving instance Read (U2 q p)

newtype M2 (i :: Type) (c :: Meta) (f :: (Type -> Type) -> k -> Type) (q :: Type -> Type) (p :: k)
    = M2 { unM2 :: f q p }

deriving instance Eq (f q p) => Eq (M2 i c f q p)
deriving instance Ord (f q p) => Ord (M2 i c f q p)
deriving instance Show (f q p) => Show (M2 i c f q p)
deriving instance Read (f q p) => Read (M2 i c f q p)

newtype K2 c (q :: Type -> Type) (p :: k)
    = K2 { unK2 :: q c }

deriving instance Eq (q c) => Eq (K2 c q p)
deriving instance Ord (q c) => Ord (K2 c q p)
deriving instance Show (q c) => Show (K2 c q p)
deriving instance Read (q c) => Read (K2 c q p)

infixr 5 :++:
data ((f :: (Type -> Type) -> k -> Type) :++: (g :: (Type -> Type) -> k -> Type)) (q :: Type -> Type) (p :: k)
    = L2 (f q p)
    | R2 (g q p)

deriving instance (Eq (f q p), Eq (g q p)) => Eq ((f :++: g) q p)
deriving instance (Ord (f q p), Ord (g q p)) => Ord ((f :++: g) q p)
deriving instance (Show (f q p), Show (g q p)) => Show ((f :++: g) q p)
deriving instance (Read (f q p), Read (g q p)) => Read ((f :++: g) q p)

infixr 6 :**:
data ((f :: (Type -> Type) -> k -> Type) :**: (g :: (Type -> Type) -> k -> Type)) (q :: Type -> Type) (p :: k)
    = f q p :**: g q p

deriving instance (Eq (f q p), Eq (g q p)) => Eq ((f :**: g) q p)
deriving instance (Ord (f q p), Ord (g q p)) => Ord ((f :**: g) q p)
deriving instance (Show (f q p), Show (g q p)) => Show ((f :**: g) q p)
deriving instance (Read (f q p), Read (g q p)) => Read ((f :**: g) q p)

-- https://gitlab.haskell.org/ghc/ghc/-/issues/15969#note_164233 !!!
infixl 7 :@@:
newtype ((f :: (Type -> Type) -> k2 -> Type) :@@: (g :: k1 -> k2)) (q :: Type -> Type) (p :: k1)
    = App2 { unApp2 :: f q (g p) }

deriving instance Eq (f q (g p)) => Eq ((f :@@: g) q p)
deriving instance Ord (f q (g p)) => Ord ((f :@@: g) q p)
deriving instance Show (f q (g p)) => Show ((f :@@: g) q p)
deriving instance Read (f q (g p)) => Read ((f :@@: g) q p)

newtype Par2 (q :: Type -> Type) (p :: Type)
    = Par2 { unPar2 :: q p }

deriving instance Eq (q p) => Eq (Par2 q p)
deriving instance Ord (q p) => Ord (Par2 q p)
deriving instance Show (q p) => Show (Par2 q p)
deriving instance Read (q p) => Read (Par2 q p)

-------------------------------------------------------------------------------
-- Synonyms for convenience
-------------------------------------------------------------------------------

type D2 = M2 D
type C2 = M2 C
type S2 = M2 S

-------------------------------------------------------------------------------
-- Generic type class
-------------------------------------------------------------------------------

class Generic (a :: Type) where
    type Rep a :: (Type -> Type) -> Type -> Type
    type Rep a = Translate (GHC.Rep a)

    to   :: Quote q => Rep a (Code q) x -> Code q a
    from :: Quote q => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r

class Generic1 (f :: k -> Type) where
    type Rep1 f :: (Type -> Type) -> k -> Type
    type Rep1 f = Translate (GHC.Rep1 f)

    to1   :: Quote q => Rep1 f (Code q) x -> Code q (f x)
    from1 :: Quote q => Code q (f x) -> (Rep1 f (Code q) x -> Code q r) -> Code q r

-------------------------------------------------------------------------------
-- Derive our Rep from GHC.Generics.Rep
-------------------------------------------------------------------------------

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
    Translate (f GHC.:.: g)  = TranslateComp (Par2 :@@: f) g
    Translate x              = TypeError ('Text "Translate error: " ':<>: 'ShowType x)

type family TranslateComp (f :: (k -> Type) -> k1 -> Type) (g :: k2 -> k1) :: (Type -> Type) -> k -> Type where
    TranslateComp acc (f GHC.:.: g) = TranslateComp (acc :@@: f) g
    TranslateComp acc (GHC.Rec1 f)  = acc :@@: f
    TranslateComp _   x             = TypeError ('Text "Translate :.: error: " ':<>: 'ShowType x)

-------------------------------------------------------------------------------
-- Example instance(s)
-------------------------------------------------------------------------------

instance Generic Bool where
    -- type Rep Bool = D2 ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
    --                    (C2 ('MetaCons "False" 'PrefixI 'False) U2 :++:
    --                     C2 ('MetaCons "True" 'PrefixI 'False) U2)

    to (M2 (L2 (M2 U2))) = [|| False ||]
    to (M2 (R2 (M2 U2))) = [|| True  ||]

    from x k = [||
        case $$x of
            False -> $$(k (M2 (L2 (M2 U2))))
            True  -> $$(k (M2 (R2 (M2 U2)))) ||]
