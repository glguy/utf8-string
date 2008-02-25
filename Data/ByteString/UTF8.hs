module Data.ByteString.UTF8
  ( B.ByteString
  , decode
  , replacement_char
  , uncons
  , splitAt
  , take
  , drop
  , span
  , break
  , fromString
  , foldl
  , foldr
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import Prelude hiding (take,drop,splitAt,span,break,foldr,foldl)

import Codec.Binary.UTF8.String(encode)

-- | Converts a Haskell string into a UTF8 encoded bytestring.
fromString :: String -> B.ByteString
fromString xs = B.pack (encode xs)

-- | Replaces malformed characters with '\xFFFD' (zero width space)
replacement_char :: Maybe Char -> Char
replacement_char Nothing  = '\xfffd'
replacement_char (Just x) = x

-- | Try to extract a character from a byte string.
-- Returns 'Nothing' if there are no more bytes in the byte string.
-- Otherwise it returns a (possibly) decoded character and the number of
-- bytes used in its representation.
decode :: B.ByteString -> Maybe (Maybe Char,Int)
decode bs = do (c,cs) <- B.uncons bs
               return (choose c cs)
  where
  choose :: Word8 -> B.ByteString -> (Maybe Char, Int)
  choose c cs
    | c < 0x80  = (Just $ toEnum $ fromEnum c, 1)
    | c < 0xc0  = (Nothing, 1)
    | c < 0xe0  = multi_byte 0x0000080 1 1 cs (mask c 0x1f)
    | c < 0xf0  = multi_byte 0x0000800 2 1 cs (mask c 0x0f)
    | c < 0xf8  = multi_byte 0x0010000 3 1 cs (mask c 0x07)
    | c < 0xfc  = multi_byte 0x0200000 4 1 cs (mask c 0x03)
    | c < 0xfe  = multi_byte 0x4000000 5 1 cs (mask c 0x01)
    | otherwise = (Nothing, 1)

  mask :: Word8 -> Word8 -> Int
  mask c m = fromEnum (c .&. m)

  multi_byte :: Int -> Int -> Int -> B.ByteString -> Int -> (Maybe Char,Int)
  multi_byte overlong 0 m _ acc
    | overlong <= acc && acc <= 0x10ffff &&
      (acc < 0xd800 || 0xdfff < acc)     &&
      (acc < 0xfffe || 0xffff < acc)      = (Just (toEnum acc),m)
    | otherwise                           = (Nothing,m)

  multi_byte overlong n m rs acc =
    case B.uncons rs of
      Just (r,rs1)
        | r .&. 0xc0 == 0x80 -> multi_byte overlong (n-1) (m+1) rs1
                              $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        | otherwise -> (Nothing,m)
      Nothing -> (Nothing,m)

-- | Split after a given number of characters.
-- Negative values are treated as if they are 0.
splitAt :: Int -> B.ByteString -> (B.ByteString,B.ByteString)
splitAt n bs = loop 0 n bs
  where loop a n _ | n <= 0 = B.splitAt a bs
        loop a n bs1 = case decode bs1 of
                         Just (_,x) -> loop (a+x) (n-1) (B.drop x bs1)
                         Nothing    -> (bs, B.empty)

-- | @take n s@ returns the first @n@ characters of @s@.
-- If @s@ has less then @n@ characters, then we return the whole of @s@.
take :: Int -> B.ByteString -> B.ByteString
take n bs = fst (splitAt n bs)

-- | @drop n s@ returns the @s@ without its first @n@ characters.
-- If @s@ has less then @n@ characters, then we return the an empty string.
drop :: Int -> B.ByteString -> B.ByteString
drop n bs = snd (splitAt n bs)

span :: (Char -> Bool) -> B.ByteString -> (B.ByteString, B.ByteString)
span p bs = loop 0 bs
  where loop a cs = case decode cs of
                      Just (c,n) | p (replacement_char c) ->
                                                  loop (a+n) (B.drop n cs)
                      _ -> B.splitAt a bs

break :: (Char -> Bool) -> B.ByteString -> (B.ByteString, B.ByteString)
break p bs = span (not . p) bs

uncons :: B.ByteString -> Maybe (Char,B.ByteString)
uncons bs = do (c,n) <- decode bs
               return (replacement_char c, drop n bs)

foldr :: (Char -> a -> a) -> a -> B.ByteString -> a
foldr cons nil cs = case uncons cs of
                      Just (a,as) -> cons a (foldr cons nil as)
                      Nothing     -> nil

foldl :: (a -> Char -> a) -> a -> B.ByteString -> a
foldl add acc cs  = case uncons cs of
                      Just (a,as) -> foldl add (add acc a) as
                      Nothing     -> acc
