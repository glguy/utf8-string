import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Codec.Binary.UTF8.String as List

main = main3

main1 = do txt <- BS.readFile "test"
           print (UTF8.length txt)

main2 = do txt <- readFile "test"
           print (length $ List.decodeString txt)

main3 = print encodeDecodeTest

encodeDecodeTest :: (String,String)
encodeDecodeTest = (filter (\x -> [x] /= f x) legal_codepoints,
                   filter (\x -> ['\xfffd'] /= f x) illegal_codepoints)
  where
    legal_codepoints = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
    illegal_codepoints = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']

    f x = UTF8.toString (UTF8.fromString [x])
