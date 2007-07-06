module Codec.Binary.UTF8.String
        ( encode
        , decode
        ) where

import Control.Monad (liftM)
import Data.Word (Word8)
import Data.Bits ((.|.),(.&.),shiftR,shiftL)
import Data.Char(chr,ord)

replacement_character :: Char
replacement_character = '\xfffd'

encode :: String -> [Word8]
encode xs = concatMap (map fromIntegral . go . ord) xs
 where
  go oc 
   | oc <= 0x7f       = [oc]
   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0x1fffff   = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0x3ffffff  = [ 0xf8 + (oc `shiftR` 24)
                        , 0x80 + ((oc `shiftR` 18) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0x7fffffff = [ 0xfc + (oc `shiftR` 30)
                        , 0x80 + ((oc `shiftR` 24) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 18) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR`  6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise = error ("Unicode character out of range" ++ show oc)

decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacement_character : decode cs
  | c < 0xe0  = multi_byte 1 0x1f 0x80
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decode cs
  where
    multi_byte n mask overlong = aux n cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacement_character : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decode rs

