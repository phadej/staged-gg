{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
-- {-# OPTIONS_GHC -ddump-splices -dsuppress-module-prefixes #-}

-- | This module is a modified version of Generics.SOP.Instances.
-- mostly the same.
module Staged.GHC.Generics.Instances () where

import Staged.GHC.Generics.TH

import Control.Exception
import Data.Char
import Data.Complex
import Data.Data
import Data.Fixed
import Data.Functor.Compose
import qualified Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.List.NonEmpty
import qualified Data.Monoid
import Data.Ord
import qualified Data.Semigroup
import Data.Version
import Data.Void
import Foreign.C.Error
import Foreign.C.Types
import GHC.ByteOrder
import GHC.Conc
import GHC.ExecutionStack
import GHC.Exts
-- import GHC.Events -- platform-specific, omitted
import GHC.Fingerprint
import GHC.Float
import qualified GHC.Generics
import GHC.IO.Buffer
import GHC.IO.Device
import GHC.IO.Encoding
import GHC.IO.Encoding.Failure
import GHC.IO.Exception
import GHC.IO.Handle
import GHC.RTS.Flags
import qualified GHC.Stack
import GHC.StaticPtr
import GHC.Stats
import System.Console.GetOpt
import System.IO
import Text.Printf
import Text.Read.Lex

-- Types from the Prelude:

-- there is manual instance for this
-- deriveGeneric ''Bool

-- Caution! While [a] may seem like a good type to use as an example for a
-- mostly-hand-written or GHC.Generic-derived instance, it should not be used
-- so (at least as of GHC 9.2.1). GHC's Rep/Rep1 produce incorrect metadata for
-- the (:) constructor. See GHC Gitlab issue #20994.
deriveGeneric ''[]
deriveGeneric ''Ordering
deriveGeneric ''Maybe
deriveGeneric ''Either
deriveGeneric ''()
deriveGeneric ''(,)              -- 2
deriveGeneric ''(,,)
deriveGeneric ''(,,,)
deriveGeneric ''(,,,,)           -- 5
deriveGeneric ''(,,,,,)
-- deriveGeneric ''(,,,,,,)
-- deriveGeneric ''(,,,,,,,)
-- deriveGeneric ''(,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,)      -- 10
-- deriveGeneric ''(,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,) -- 15
-- deriveGeneric ''(,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,) -- 20
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,) -- 25
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
-- deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) -- 30

deriveGeneric1 ''Maybe
deriveGeneric1 ''Either
deriveGeneric1 ''(,)              -- 2
deriveGeneric1 ''(,,)
deriveGeneric1 ''(,,,)
deriveGeneric1 ''(,,,,)           -- 5
deriveGeneric1 ''(,,,,,)
deriveGeneric1 ''(,,,,,,)
deriveGeneric1 ''(,,,,,,,)
deriveGeneric1 ''(,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,)      -- 10
deriveGeneric1 ''(,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,) -- 15
deriveGeneric1 ''(,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,) -- 20
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,) -- 25
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric1 ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) -- 30
deriveGeneric1 ''[]

-- Other types from base:

-- From Control.Exception:
deriveGeneric ''IOException
deriveGeneric ''ArithException
deriveGeneric ''ArrayException
deriveGeneric ''AssertionFailed
deriveGeneric ''AsyncException
deriveGeneric ''NonTermination
deriveGeneric ''NestedAtomically
deriveGeneric ''BlockedIndefinitelyOnMVar
deriveGeneric ''BlockedIndefinitelyOnSTM
deriveGeneric ''AllocationLimitExceeded
deriveGeneric ''Deadlock
deriveGeneric ''NoMethodError
deriveGeneric ''PatternMatchFail
deriveGeneric ''RecConError
deriveGeneric ''RecSelError
deriveGeneric ''RecUpdError
deriveGeneric ''ErrorCall
deriveGeneric ''TypeError
deriveGeneric ''MaskingState

-- From Data.Char:
deriveGeneric ''GeneralCategory

-- From Data.Complex:
deriveGeneric ''Complex

-- From Data.Data:
deriveGeneric ''DataRep
deriveGeneric ''Fixity
deriveGeneric ''ConstrRep

-- From Data.Fixed:
deriveGeneric ''Fixed
deriveGeneric ''E0
deriveGeneric ''E1
deriveGeneric ''E2
deriveGeneric ''E3
deriveGeneric ''E6
deriveGeneric ''E9
deriveGeneric ''E12

-- From Data.Functor.Compose
deriveGeneric ''Compose
deriveGeneric1 ''Compose

-- From Data.Functor.Const
deriveGeneric ''Data.Functor.Const.Const
deriveGeneric1 ''Data.Functor.Const.Const

-- From Data.Functor.Identity
deriveGeneric ''Identity
deriveGeneric1 ''Identity

-- From Data.Functor.Product
deriveGeneric ''Product
deriveGeneric1 ''Product

-- From Data.Functor.Sum
deriveGeneric ''Sum
deriveGeneric1 ''Sum

-- From Data.List.NonEmpty
deriveGeneric ''NonEmpty
deriveGeneric1 ''NonEmpty

-- From Data.Monoid:
deriveGeneric ''Data.Monoid.Dual
deriveGeneric ''Data.Monoid.Endo
deriveGeneric ''Data.Monoid.All
deriveGeneric ''Data.Monoid.Any
deriveGeneric ''Data.Monoid.Sum
deriveGeneric ''Data.Monoid.Product
deriveGeneric ''Data.Monoid.First
deriveGeneric ''Data.Monoid.Last
deriveGeneric ''Data.Monoid.Alt

deriveGeneric1 ''Data.Monoid.Dual
-- deriveGeneric1 ''Data.Monoid.Endo
deriveGeneric1 ''Data.Monoid.Sum
deriveGeneric1 ''Data.Monoid.Product
deriveGeneric1 ''Data.Monoid.First
deriveGeneric1 ''Data.Monoid.Last
deriveGeneric1 ''Data.Monoid.Alt

-- From Data.Ord:
deriveGeneric ''Down
deriveGeneric1 ''Down

-- From Data.Proxy:
deriveGeneric ''Proxy
deriveGeneric1 ''Proxy

-- From Data.Semigroup:
deriveGeneric ''Data.Semigroup.Min
deriveGeneric ''Data.Semigroup.Max
deriveGeneric ''Data.Semigroup.First
deriveGeneric ''Data.Semigroup.Last
deriveGeneric ''Data.Semigroup.WrappedMonoid
deriveGeneric ''Data.Semigroup.Option
deriveGeneric ''Data.Semigroup.Arg

deriveGeneric1 ''Data.Semigroup.Min
deriveGeneric1 ''Data.Semigroup.Max
deriveGeneric1 ''Data.Semigroup.First
deriveGeneric1 ''Data.Semigroup.Last
deriveGeneric1 ''Data.Semigroup.WrappedMonoid
deriveGeneric1 ''Data.Semigroup.Option
deriveGeneric1 ''Data.Semigroup.Arg

-- From Data.Version:
deriveGeneric ''Version

-- From Data.Void:
deriveGeneric ''Void

-- From Foreign.C.Error:
deriveGeneric ''Errno

-- From Foreign.C.Types:
deriveGeneric ''CChar
deriveGeneric ''CSChar
deriveGeneric ''CUChar
deriveGeneric ''CShort
deriveGeneric ''CUShort
deriveGeneric ''CInt
deriveGeneric ''CUInt
deriveGeneric ''CLong
deriveGeneric ''CULong
deriveGeneric ''CPtrdiff
deriveGeneric ''CSize
deriveGeneric ''CWchar
deriveGeneric ''CSigAtomic
deriveGeneric ''CLLong
deriveGeneric ''CULLong
deriveGeneric ''CIntPtr
deriveGeneric ''CUIntPtr
deriveGeneric ''CIntMax
deriveGeneric ''CUIntMax
deriveGeneric ''CClock
deriveGeneric ''CTime
deriveGeneric ''CUSeconds
deriveGeneric ''CSUSeconds
deriveGeneric ''CFloat
deriveGeneric ''CDouble

-- From GHC.ByteOrder:
deriveGeneric ''ByteOrder

-- From GHC.Conc:
deriveGeneric ''ThreadStatus
deriveGeneric ''BlockReason

-- From GHC.ExecutionStack:
deriveGeneric ''Location
deriveGeneric ''SrcLoc

-- From GHC.Exts:
deriveGeneric ''RuntimeRep
deriveGeneric ''VecCount
deriveGeneric ''VecElem

-- From GHC.Generics:
deriveGeneric ''GHC.Generics.K1
deriveGeneric ''GHC.Generics.U1
deriveGeneric ''GHC.Generics.V1
deriveGeneric ''GHC.Generics.Par1
deriveGeneric ''GHC.Generics.M1
deriveGeneric ''GHC.Generics.R
deriveGeneric ''GHC.Generics.S
deriveGeneric ''GHC.Generics.D
deriveGeneric ''GHC.Generics.C
deriveGeneric ''(GHC.Generics.:*:)
deriveGeneric ''(GHC.Generics.:+:)
deriveGeneric ''(GHC.Generics.:.:)
deriveGeneric ''GHC.Generics.Associativity
deriveGeneric ''GHC.Generics.DecidedStrictness
deriveGeneric ''GHC.Generics.SourceStrictness
deriveGeneric ''GHC.Generics.SourceUnpackedness
deriveGeneric ''GHC.Generics.Fixity

-- From GHC.IO.Buffer:
deriveGeneric ''Buffer
deriveGeneric ''BufferState

-- From GHC.IO.Device:
deriveGeneric ''IODeviceType

-- From GHC.IO.Encoding:
deriveGeneric ''BufferCodec
deriveGeneric ''CodingProgress

-- From GHC.IO.Encoding.Failure:
deriveGeneric ''CodingFailureMode

-- From GHC.Fingerprint
deriveGeneric ''Fingerprint

-- From GHC.Float
deriveGeneric ''FFFormat

-- From GHC.IO.Exception:
deriveGeneric ''FixIOException
deriveGeneric ''IOErrorType

-- From GHC.IO.Handle:
deriveGeneric ''HandlePosn
deriveGeneric ''LockMode

-- From GHC.RTS.Flags:
deriveGeneric ''RTSFlags
deriveGeneric ''GiveGCStats
deriveGeneric ''GCFlags
deriveGeneric ''ConcFlags
deriveGeneric ''MiscFlags
deriveGeneric ''DebugFlags
deriveGeneric ''DoCostCentres
deriveGeneric ''CCFlags
deriveGeneric ''DoHeapProfile
deriveGeneric ''ProfFlags
deriveGeneric ''DoTrace
deriveGeneric ''TraceFlags
deriveGeneric ''TickyFlags
deriveGeneric ''ParFlags

-- From GHC.Stack:
deriveGeneric ''GHC.Stack.SrcLoc
deriveGeneric ''GHC.Stack.CallStack

-- From GHC.StaticPtr:
deriveGeneric ''StaticPtrInfo

-- From GHC.Stats:
deriveGeneric ''RTSStats
deriveGeneric ''GCDetails

-- From System.Console.GetOpt:

deriveGeneric ''ArgOrder
deriveGeneric ''OptDescr
deriveGeneric ''ArgDescr

-- From System.Exit:

deriveGeneric ''ExitCode

-- From System.IO:

deriveGeneric ''IOMode
deriveGeneric ''BufferMode
deriveGeneric ''SeekMode
deriveGeneric ''Newline
deriveGeneric ''NewlineMode

-- From Text.Printf:

deriveGeneric ''FieldFormat
deriveGeneric ''FormatAdjustment
deriveGeneric ''FormatSign
deriveGeneric ''FormatParse

-- From Text.Read.Lex:

deriveGeneric ''Lexeme
deriveGeneric ''Number

-- Abstract / primitive datatypes (we don't derive Generic for these):
--
-- Ratio
-- Integer
-- ThreadId
-- Chan
-- MVar
-- QSem
-- QSemN
-- DataType
-- Dynamic
-- IORef
-- TypeRep
-- TyCon
-- TypeRepKey
-- KProxy -- not abstract, but intended for kind-level use
-- STRef
-- Unique
-- ForeignPtr
-- CFile
-- CFpos
-- CJmpBuf
-- Pool
-- Ptr
-- FunPtr
-- IntPtr
-- WordPtr
-- StablePtr
-- Char
-- Double
-- Float
-- Int
-- Int8
-- Int16
-- Int32
-- Int64
-- Word
-- Word8
-- Word16
-- Word32
-- Word64
-- IO
-- ST
-- (->)
-- RealWorld
-- Handle
-- HandlePosn
-- TextEncoding
-- StableName
-- Weak
-- ReadP
-- ReadPrec
-- STM
-- TVar
-- Natural
-- Event
-- EventManager
-- CostCentre
-- CostCentreStack
--
-- Datatypes we cannot currently handle:
--
-- SomeException
-- SomeAsyncException
-- Handler
-- Coercion
-- (:~:)
