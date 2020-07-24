module Foreign.C.Convertable
  ( Convertable (..),
    module Foreign,
    module Foreign.C.Types,
  )
where

import Foreign
import Foreign.C.Types

-- | A value that can be converted to and from a C type
class Convertable c haskell where
  toC :: haskell -> c
  fromC :: c -> haskell
  {-# MINIMAL toC, fromC #-}
