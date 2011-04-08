module Data.ByteString.Lazy.UTF8.IsString () where

import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 as B8
import Data.String

instance IsString ByteString where
    {-# INLINE fromString #-}
    fromString = B8.fromString
