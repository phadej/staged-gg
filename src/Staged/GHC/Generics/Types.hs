{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- For default Rep definition
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK --not-home #-}

-- | This module contains the definitions of the 'Generic' and 'Generic1'
-- classes. Caution: it does /not/ expose the instances! It is intended
-- primarily for internal use.
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
    GRep,
    genericTo,
    genericFrom,
    GRep1,
    genericTo1,
    genericFrom1,
    -- * TH Types
    Code, Quote,
    -- * Utilities
    Translate,
) where

import Data.Kind           (Type)
import Staged.GHC.Generics.RepTypes
import Staged.GHC.Generics.Generic
    ( GGeneric
    , GRep
    , genericTo
    , genericFrom
    , GRep1
    , genericTo1
    , genericFrom1
    )

import qualified GHC.Generics as GHC
import qualified Staged.GHC.Generics.FakeGeneric1 as Fake

-------------------------------------------------------------------------------
-- Generic type class
-------------------------------------------------------------------------------

class Generic (a :: Type) where
    type Rep a :: (Type -> Type) -> Type -> Type
    type Rep a = GRep a

    to   :: Quote q => Rep a (Code q) x -> Code q a
    default to :: (Rep a ~ GRep a, GHC.Generic a, GGeneric (Rep a), Quote q)
               => Rep a (Code q) x -> Code q a
    to = genericTo

    from :: Quote q => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
    default from :: (Rep a ~ GRep a, GHC.Generic a, GGeneric (Rep a), Quote q)
      => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
    from = genericFrom

class Generic1 (f :: k -> Type) where
    type Rep1 f :: (Type -> Type) -> k -> Type
    type Rep1 f = GRep1 f

    to1   :: Quote q => Rep1 f (Code q) x -> Code q (f x)
    default to1 :: (Rep1 f ~ GRep1 f, Fake.Generic1 f, GGeneric (Rep1 f), Quote q)
      => Rep1 f (Code q) x -> Code q (f x)
    to1 = genericTo1

    from1 :: Quote q => Code q (f x) -> (Rep1 f (Code q) x -> Code q r) -> Code q r
    default from1 :: (Rep1 f ~ GRep1 f, Fake.Generic1 f, GGeneric (Rep1 f), Quote q)
      => Code q (f x) -> (Rep1 f (Code q) x -> Code q r) -> Code q r
    from1 = genericFrom1

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
