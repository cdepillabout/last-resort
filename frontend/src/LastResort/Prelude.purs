
module LastResort.Prelude
  ( module Debug.Trace
  , module Prelude
  , undefined
  ) where

import Prelude

import Debug.Trace
  (class DebugWarning, spy, trace, traceA, traceAny, traceAnyA, traceAnyM,
   traceShow, traceShowA, traceShowM)
import Unsafe.Coerce (unsafeCoerce)

undefined :: forall a. a
undefined = unsafeCoerce unit
