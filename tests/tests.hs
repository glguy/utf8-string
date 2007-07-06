main = do
  putStrLn $ "Encode Decode: " ++ show encodeDecodeTest
  
encodeDecodeTest = all (\x -> x == decode (encode x)) legal_codepoints
                && all (\x -> '\xfffd' == decode (encode x)) illegal_codepoints
  where
    legal_codepoints = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
    illegal_codepoints = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff'] ++ ['\x10000'..'\x10ffff']
