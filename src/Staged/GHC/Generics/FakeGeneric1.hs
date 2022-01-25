-- ---------------------------
-- Author: David Feuer
-- Copyright: 2021, 2022 David Feuer
{-# language DefaultSignatures #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-} -- For the DerivingVia target
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}

-- | A modern version of @"GHC.Generics".'GHC.Generics.Generic1'@, and a custom
-- copy of ':.:' to go with it. Unlike the GHC version,
--
-- 1. The ':.:' operator here associates to the /left/, rather than the right,
-- in representations. 
--
-- 2. Instances never require 'Functor' constraints, and you can generally
-- avoid extraneous 'Functor' instances and 'fmap' applications to use them.
--
-- 3. Kind arguments are inferred, not specified. This may eventually be true
-- for GHC generics too. Who knows?
--
-- 4. The default intances here are likely to optimize even worse than with GHC
-- generics.  Yay! We do this in a principled way. @linear-generics@, which
-- uses a similar approach, cheats with @unsafeCoerce@ (which it needs anyway
-- for linearity hocus pocus). Both approaches are almost certain to be bad; I
-- don't know which is worse.

module Staged.GHC.Generics.FakeGeneric1
  ( Generic1 (..)
  , (:.:) (..)
  , GHCGenerically1 (..)
  ) where
import GHC.Generics hiding (Generic1 (..))
import Data.Kind
import Data.Coerce
import GHC.TypeLits

-- For instances
import qualified Control.Applicative
import qualified Data.Semigroup
import qualified Data.Monoid
import qualified Control.Arrow
import qualified Data.Proxy
import qualified Data.Ord
import qualified Data.List.NonEmpty
import qualified Data.Functor.Sum
import qualified Data.Functor.Product
import qualified Data.Functor.Compose
import qualified Data.Functor.Const
import qualified Data.Functor.Identity
import qualified Data.Complex

type Generic1 :: forall {k}. (k -> Type) -> Constraint
class Generic1 (f :: k -> Type) where
  type Rep1 f :: k -> Type
  type Rep1 f = GRep1 f

  to1 :: forall a. Rep1 f a -> f a
  default to1 :: (GenericQ f a, Rep1 f ~ GRep1 f) => Rep1 f a -> f a
  to1 = toQ

  from1 :: f a -> Rep1 f a
  default from1 :: (GenericQ f a, Rep1 f ~ GRep1 f) => f a -> Rep1 f a
  from1 = fromQ

-- | A 'DerivingVia' target for deriving 'Generic1' instances, for those who
-- like to be explicit. This type, by its very purpose, has a mismatched
-- 'Generic1' instance, so it probably shouldn't be used for other purposes.
type GHCGenerically1 :: forall {k}. (k -> Type) -> k -> Type
newtype GHCGenerically1 f a = GHCGenerically1 { unGHCGenerically1 :: f a }

instance forall k (f :: k -> Type).
  (forall (a :: k). GenericQ f a) => Generic1 (GHCGenerically1 f) where
  type Rep1 (GHCGenerically1 (f :: k -> Type)) = GRep1 f

  to1 :: GRep1 f a -> GHCGenerically1 f a
  to1 = GHCGenerically1 #. toQ

  from1 :: forall a. GHCGenerically1 f a -> Rep1 (GHCGenerically1 f) a
  from1 = fromQ .# unGHCGenerically1

type GenericQ :: forall {k}. (k -> Type) -> k -> Constraint
class GenericQ (f :: k -> Type) (a :: k) where
  toQ :: GRep1 f a -> f a
  fromQ :: f a -> GRep1 f a

instance forall k (f :: k -> Type) (a :: k) rfa r1f.
  (Generic1' rfa r1f a, rfa ~ Rep (f a), r1f ~ GRep1 f, Generic (f a)) => GenericQ f a where
  toQ :: GRep1 f a -> f a
  toQ = to . from1' @rfa @r1f @a

  fromQ :: f a -> GRep1 f a
  fromQ = to1' @rfa @r1f @a . from

type Generic1' :: forall {k}. (Type -> Type) -> (k -> Type) -> k -> Constraint
class Generic1' (rep :: Type -> Type) (rep1 :: k -> Type) (a :: k) where
  to1' :: rep p -> rep1 a
  from1' :: rep1 a -> rep p

instance Generic1' f f' a => Generic1' (M1 i c f) (M1 i c f') a where
  to1' (M1 x) = M1 (to1' @f @f' @a x)
  from1' (M1 x) = M1 (from1' @f @f' @a x)

instance (Generic1' f f' a, Generic1' g g' a) => Generic1' (f :*: g) (f' :*: g') a where
  to1' (x :*: y) = to1' @f @f' @a x :*: to1' @g @g' @a y
  from1' (x :*: y) = from1' @f @f' @a x :*: from1' @g @g' @a y

instance (Generic1' f f' a, Generic1' g g' a) => Generic1' (f :+: g) (f' :+: g') a where
  to1' (L1 x) = L1 (to1' @f @f' @a x)
  to1' (R1 y) = R1 (to1' @g @g' @a y)
  from1' (L1 x) = L1 (from1' @f @f' @a x)
  from1' (R1 y) = R1 (from1' @g @g' @a y)

instance Generic1' U1 U1 a where
  to1' U1 = U1
  from1' U1 = U1

instance Generic1' V1 V1 a where
  to1' x = case x of
  from1' x = case x of

-- This instance is pure magic.
instance Coercible c (r a) => Generic1' (Rec0 c) r a where
  to1' (K1 x) = coerce @c @(r a) x
  from1' x = K1 (coerce @(r a) @c x)

-- We use a marking technique inspired by Csongor Kiss's `GenericN`, part of
-- his generic-lens package family. By applying `LastPar` and `OtherPar` to the
-- parameters of the type before taking its generic representation, we gain the
-- ability to recognize precisely when a type buried somewhere in the generic
-- representation is the last type parameter or some other one. As we calculate
-- the Rep1, we remove the marks. The last parameter always vanishes; the rest
-- we put back where they came from.
--
-- Note: we can't do anything like Translate1 because 
type GRep1 :: forall {k}. (k -> Type) -> k -> Type
type GRep1 (f :: k -> Type) = MakeRep1' @k (Rep ((MarkOtherPars f) (LastPar :: k)))

-- Aren't these data families disgusting? We claim to create fresh and genuine
-- inhabitants of absolutely arbitrary kinds! I don't know why GHC even allows
-- this, and I wouldn't be shocked if it stopped. In that case, it's possible to
-- do exactly the same thing using stuck type families 
data family LastPar :: k
data family OtherPar :: k -> k

type MakeRep1' :: forall k. (Type -> Type) -> k -> Type
type family MakeRep1' (rep :: Type -> Type) :: k -> Type where
  MakeRep1' (M1 i c f) = M1 i c (MakeRep1' f)
  MakeRep1' (x :+: y) = MakeRep1' x :+: MakeRep1' y
  MakeRep1' (x :*: y) = MakeRep1' x :*: MakeRep1' y
  MakeRep1' U1 = U1
  MakeRep1' V1 = V1
  MakeRep1' (Rec0 c) = MakeRep1Field (Rec0 (Unmark c)) Par1 c

type MarkOtherPars :: forall k. k -> k
type family MarkOtherPars (f :: k) :: k where
  MarkOtherPars ((f :: j -> k) (a :: j)) = MarkOtherPars f (OtherPar a)
  MarkOtherPars f = f

type Unmark :: forall k. k -> k
type family Unmark (f :: k) :: k where
  Unmark LastPar = TypeError
    ('Text "Cannot create Generic1 instance: the last parameter appears in an invalid location.")
  Unmark (OtherPar a) = a
  Unmark ((f :: j -> k) (a :: j)) = Unmark f (Unmark a)
  Unmark a = a

type MakeRep1Field :: forall j k. (k -> Type) -> (j -> Type) -> j -> k -> Type
type family MakeRep1Field fk acc c where
  MakeRep1Field fk (_ :: b -> Type) (OtherPar _) = fk
  MakeRep1Field fk (acc :: b -> Type) ((f :: a -> b) (x :: a)) =
      MakeRep1Field fk (acc :.: Unmark f) x
  MakeRep1Field fk (acc :: k -> Type) (LastPar :: k) = acc
  MakeRep1Field fk _ _ = fk

-- Stolen from profunctors

infixr 9 #.
(#.) :: Coercible b c => p b c -> (a -> b) -> a -> c
(#.) _ = coerce

infixl 8 .#
(.#) :: Coercible a b => (b -> c) -> p a b -> a -> c
f .# _ = coerce f


-- ------------------
--
-- Instances
--
-- ------------------

instance Generic1 Maybe
instance Generic1 (Either a)

-- GHC only gives us Generic instances for tuples up to 7. If someone wants
-- more tuple instances for this Generic1, they'll want to borrow the Template
-- Haskell from linear-generics. Have fun!
instance Generic1 ((,) a)
instance Generic1 ((,,) a b)
instance Generic1 ((,,,) a b c)
instance Generic1 ((,,,,) a b c d)
instance Generic1 ((,,,,,) a b c d e)
instance Generic1 ((,,,,,,) a b c d e f) -- 7

-- Using the default instance, the (:) constructor will have the wrong fixity
-- metadata in GHC 9.2.1 and earlier. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/20994
-- Ryan Scott has just submitted a small MR to fix it, so I assume it will be
-- fixed soon.
instance Generic1 [] where
  type Rep1 [] =
    D1
      ('MetaData "[]" "GHC.Types" "ghc-prim" 'False)
      (C1
         ('MetaCons "[]" 'PrefixI 'False)
         U1 :+: C1
                  ('MetaCons ":" ('InfixI 'RightAssociative 5) 'False)
                     (S1
                        ('MetaSel
                         'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
                        Par1 :*: S1
                                   ('MetaSel
                                    'Nothing
                                    'NoSourceUnpackedness
                                    'NoSourceStrictness
                                    'DecidedLazy)
                                   (Par1 :.: [])))
  {-# INLINE to1 #-}
  to1 (M1 (L1 (M1 U1))) = []
  to1 (M1 (R1 (M1 (M1 (Par1 x) :*: M1 (Comp1 (Par1 xs)))))) = x : xs

  {-# INLINE from1 #-}
  from1 [] = M1 (L1 (M1 U1))
  from1 (x : xs) = M1 (R1 (M1 (M1 (Par1 x) :*: M1 (Comp1 (Par1 xs)))))

instance Generic1 (Control.Applicative.WrappedArrow m a)
instance Generic1 Data.Complex.Complex
instance Generic1 (Data.Functor.Compose.Compose f g)
instance Generic1 (Data.Functor.Const.Const a)
instance Generic1 Data.Functor.Identity.Identity
instance Generic1 (Data.Functor.Product.Product f g)
instance Generic1 (Data.Functor.Sum.Sum f g)
instance Generic1 Data.List.NonEmpty.NonEmpty
instance Generic1 Data.Monoid.Dual
--instance Generic1 Data.Monoid.Endo  -- Impossible without more representation types
instance Generic1 (Control.Arrow.Kleisli m a)
instance Generic1 Data.Monoid.Sum
instance Generic1 Data.Monoid.Product
instance Generic1 Data.Monoid.First
instance Generic1 Data.Monoid.Last
instance Generic1 (Data.Monoid.Alt f)
instance Generic1 (Data.Monoid.Ap f)
instance Generic1 Data.Ord.Down
instance Generic1 Data.Proxy.Proxy
instance Generic1 Data.Semigroup.Min
instance Generic1 Data.Semigroup.Max
instance Generic1 Data.Semigroup.First
instance Generic1 Data.Semigroup.Last
instance Generic1 Data.Semigroup.WrappedMonoid
instance Generic1 (Data.Semigroup.Arg a)

instance Generic1 (K1 i c)
instance Generic1 U1
instance Generic1 V1
instance Generic1 Par1
instance Generic1 (M1 i c f)
instance Generic1 (f :*: g)
instance Generic1 (f :+: g)
instance Generic1 (f :.: g)
