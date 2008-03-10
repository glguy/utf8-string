import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.String.UTF8     as UTF8

import System.IO
import Data.Word

main  = main4

main1 = do putStrLn "Speed: Data.ByteString"
           txt <- S.readFile "test"
           print (UTF8.length $ UTF8.fromRep txt)

main2 = do putStrLn "Speed: Data.ByteString.Lazy"
           txt <- L.readFile "test"
           print (UTF8.length $ UTF8.fromRep txt)

main3 = do putStrLn "Speed: [Word8]"
           txt <- hGetContents =<< openBinaryFile "test" ReadMode
           let bytes :: [Word8]
               bytes = map (fromIntegral . fromEnum) txt
           print (UTF8.length $ UTF8.fromRep bytes)

main4 = do putStrLn "Correctness: Data.ByteString"
           print encodeDecodeTest

encodeDecodeTest :: String
encodeDecodeTest =
     filter (\x -> enc x /= [x]) legal_codepoints
  ++ filter (\x -> enc x /= [UTF8.replacement_char]) illegal_codepoints
  where
    legal_codepoints    = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd']
                       ++ ['\x10000'..'\x10ffff']
    illegal_codepoints  = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']

{-# INLINE enc #-}
enc x = UTF8.toString (UTF8.fromString [x] :: UTF8.UTF8 S.ByteString)


