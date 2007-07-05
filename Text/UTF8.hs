{-# OPTIONS_GHC -fno-implicit-prelude #-}
module Text.UTF8 
        ( encodeUTF8
        , decodeUTF8
        , print
        , putStrLn
        , getLine
        , readLn
        , readFile
        , writeFile
        ) where

import Control.Monad (liftM)
import Data.Word (Word8)
import Data.Bits ((.|.),(.&.),shiftR,shiftL)
import Data.Char(chr,ord)
import Prelude hiding (getLine, putStrLn, readLn, print, readFile, writeFile)
import qualified System.IO as IO

print :: Show a => a -> IO ()
print x = putStrLn (show x)

putStrLn :: String -> IO ()
putStrLn x = IO.putStrLn . bytesToString . encodeUTF8 $ x

getLine :: IO String
getLine = liftM (decodeUTF8 . stringToBytes) IO.getLine

readLn :: Read a => IO a
readLn = readIO =<< getLine

readFile :: String -> IO String
readFile n = liftM (decodeUTF8 . stringToBytes) (IO.readFile n)

writeFile :: String -> String -> IO ()
writeFile n c = IO.writeFile n . bytesToString . encodeUTF8 $ c

bytesToString :: [Word8] -> String
bytesToString xs = map (chr . fromEnum) xs

stringToBytes :: String -> [Word8]
stringToBytes xs = map (toEnum . ord) xs

encodeUTF8 :: String -> [Word8]
encodeUTF8 xs = concatMap (map fromIntegral . go . ord) xs
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

replacement_character :: Char
replacement_character = '\xfffd'

decodeUTF8        :: [Word8] -> String
decodeUTF8 [    ] = ""
decodeUTF8 (c:cs)
  | c < 0x80  = chr (fromEnum c) : decodeUTF8 cs
  | c < 0xc0  = replacement_character : decodeUTF8 cs
  | c < 0xe0  = multi_byte 1 0x1f 0x80
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decodeUTF8 cs
  where
    multi_byte n mask overlong = aux n cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decodeUTF8 rs
          | otherwise = replacement_character : decodeUTF8 rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decodeUTF8 rs

