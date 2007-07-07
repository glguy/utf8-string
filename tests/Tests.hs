import Codec.Binary.UTF8.String

main = let v = encodeDecodeTest
       in putStrLn $ case v of
            [] -> "Ok. All passed."
            s  -> "Encode/decode failures: " ++ show s

--
-- test decode . encode == id for the class of chars we know that to be true of
--
encodeDecodeTest :: [Char]
encodeDecodeTest = filter (\x -> [x] /= decode (encode [x])) legal_codepoints ++
                   filter (\x -> ['\xfffd'] /= decode (encode [x])) illegal_codepoints
  where
    legal_codepoints = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
    illegal_codepoints = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']
