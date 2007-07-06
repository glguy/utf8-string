import Codec.Binary.UTF8.String

main = do
  putStrLn $ "Encode Decode failures: " ++ show encodeDecodeTest
  
encodeDecodeTest = filter (\x -> [x] /= decode (encode [x])) legal_codepoints ++
                   filter (\x -> ['\xfffd'] /= decode (encode [x])) illegal_codepoints
  where
    legal_codepoints = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
    illegal_codepoints = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']
