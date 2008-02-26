import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Codec.Binary.UTF8.String as List

main = main1

main1 = do txt <- BS.readFile "test"
           print (UTF8.length txt)

main2 = do txt <- readFile "test"
           print (length $ List.decodeString txt)

