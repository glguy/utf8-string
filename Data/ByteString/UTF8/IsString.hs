module Data.ByteString.UTF8.IsString () where

import Data.ByteString as B
import Data.ByteString.UTF8 as B8
import Data.String

instance IsString ByteString where
    {-# INLINE fromString #-}
    fromString = B8.fromString
