module Day16 where

day16a :: String -> Int
day16a  = undefined

day16b :: String -> Int
day16b  = undefined

data Bit = One | Zero

hexToBin :: String -> [Bit]
hexToBin = go [] where
  go decoded (x:xs) = go (decoded ++ hexCharToBin x) xs
  go decoded [] = decoded

hexCharToBin :: Char -> [Bit]
hexCharToBin c = case c of
  '0' -> [Zero, Zero, Zero, Zero]
  '1' -> [Zero, Zero, Zero, One]
  '2' -> [Zero, Zero, One, Zero]
  '3' -> [Zero, Zero, One, One]
  '4' -> [Zero, One, Zero, Zero]
  '5' -> [Zero, One, Zero, One]
  '6' -> [Zero, One, One, Zero]
  '7' -> [Zero, One, One, One]
  '8' -> [One, Zero, Zero, Zero]
  '9' -> [One, Zero, Zero, One]
  'A' -> [One, Zero, One, Zero]
  'B' -> [One, Zero, One, One]
  'C' -> [One, One, Zero, Zero]
  'D' -> [One, One, Zero, One]
  'E' -> [One, One, One, Zero]
  'F' -> [One, One, One, One]
