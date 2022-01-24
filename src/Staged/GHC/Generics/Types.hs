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
    genericTo,
    genericFrom,
    -- * TH Types
    Code, Quote,
    -- * Utilities
    Translate,
) where

import Data.Kind           (Type)
import Staged.GHC.Generics.RepTypes
import Staged.GHC.Generics.GHCish (GGeneric (..), ghcGenericTo, ghcGenericFrom)

import qualified GHC.Generics as GHC

-------------------------------------------------------------------------------
-- Generic type class
-------------------------------------------------------------------------------

class Generic (a :: Type) where
    type Rep a :: (Type -> Type) -> Type -> Type
    type Rep a = Translate (GHC.Rep a)

    to   :: Quote q => Rep a (Code q) x -> Code q a
    default to :: (Rep a ~ Translate (GHC.Rep a), GHC.Generic a, GGeneric (Rep a), Quote q)
               => Rep a (Code q) x -> Code q a
    to = genericTo

    from :: Quote q => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
    default from :: (Rep a ~ Translate (GHC.Rep a), GHC.Generic a, GGeneric (Rep a), Quote q)
      => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
    from = genericFrom

-- | Use GHC generics to implement the 'to' method.
genericTo :: (Rep a ~ Translate (GHC.Rep a), GHC.Generic a, GGeneric (Rep a), Quote q)
  => Rep a (Code q) x -> Code q a
genericTo = ghcGenericTo

-- | Use GHC generics to implement the 'from' method.
genericFrom :: (Rep a ~ Translate (GHC.Rep a), GHC.Generic a, GGeneric (Rep a), Quote q)
  => Code q a -> (Rep a (Code q) x -> Code q r) -> Code q r
genericFrom = ghcGenericFrom

class Generic1 (f :: k -> Type) where
    type Rep1 f :: (Type -> Type) -> k -> Type
    type Rep1 f = Translate (GHC.Rep1 f)

    to1   :: Quote q => Rep1 f (Code q) x -> Code q (f x)
    from1 :: Quote q => Code q (f x) -> (Rep1 f (Code q) x -> Code q r) -> Code q r

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
