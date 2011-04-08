module Data.ByteString.Lazy.UTF8.IsString (UTF8Lit(..)) where

import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 as B8
import Data.String

newtype UTF8Lit = UTF8Lit { litUTF8 :: ByteString }

instance IsString UTF8Lit where
    {-# INLINE fromString #-}
    fromString = UTF8Lit . B8.fromString
