module System.IO.UTF8
        ( print
        , putStr
        , putStrLn
        , getLine
        , readLn
        , readFile
        , writeFile
        , appendFile
        , getContents
        , hGetLine
        , hGetContents
        , hPutStr
        , hPutStrLn
        ) where

import Prelude (String, (=<<), (.), map, toEnum, fromEnum, Read, Show(..))
import qualified System.IO as IO
import System.IO (Handle, IO, FilePath)
import Codec.Binary.UTF8.String (encode, decode)
import Control.Monad (liftM)
import Data.Char (ord, chr)
import Data.Word (Word8)

encodeString :: String -> String
encodeString xs = bytesToString (encode xs)

decodeString :: String -> String
decodeString xs = decode (stringToBytes xs)

bytesToString :: [Word8] -> String
bytesToString xs = map (chr . fromEnum) xs

stringToBytes :: String -> [Word8]
stringToBytes xs = map (toEnum . ord) xs

print :: Show a => a -> IO ()
print x = putStrLn (show x)

putStr :: String -> IO ()
putStr x = IO.putStr (encodeString x)

putStrLn :: String -> IO ()
putStrLn x = IO.putStrLn (encodeString x)

getLine :: IO String
getLine = liftM decodeString IO.getLine

readLn :: Read a => IO a
readLn = IO.readIO =<< getLine

readFile :: FilePath -> IO String
readFile n = liftM decodeString (IO.readFile n)

writeFile :: FilePath -> String -> IO ()
writeFile n c = IO.writeFile n (encodeString c)

appendFile :: FilePath -> String -> IO ()
appendFile n c = IO.appendFile n (encodeString c)

hGetLine :: Handle -> IO String
hGetLine h = liftM decodeString (IO.hGetLine h)

hGetContents :: Handle -> IO String
hGetContents h = liftM decodeString (IO.hGetContents h)

hPutStr :: Handle -> String -> IO ()
hPutStr h s = IO.hPutStr h (encodeString s)
 
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = IO.hPutStrLn h (encodeString s)
 
getContents :: IO String
getContents = liftM decodeString IO.getContents

