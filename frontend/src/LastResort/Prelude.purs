
module LastResort.Prelude
  ( module Prelude
  , undefined
  ) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

undefined :: forall a. a
undefined = unsafeCoerce unit
