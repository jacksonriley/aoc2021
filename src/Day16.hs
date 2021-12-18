module Day16 where

import Data.Bifunctor (first)
import Data.Bits ((.&.))
import Data.Char (isAlpha, ord)
import Data.List (foldl')
import Debug.Trace

data Bit
  = One
  | Zero
  deriving (Show)

data Packet
  = Literal Version Int
  | Operator Version Int [Packet]
  deriving (Show)

newtype Version =
  Version Int
  deriving (Show)

day16a :: String -> Int
day16a = sumVersions . fst . parsePacket . hexToBin

day16b :: String -> Int
day16b = sumVersions' . fst . parsePacket . hexToBin

hexToBin :: String -> [Bit]
hexToBin = go []
  where
    go decoded (x:xs) = go (decoded ++ hexCharToBin x) xs
    go decoded [] = decoded

decToBin :: Int -> [Bit]
decToBin num = go 8
  where
    go 0 = []
    go mask =
      (if (.&.) num mask == mask
         then One
         else Zero) :
      go (mask `div` 2)

binToDec :: [Bit] -> Int
binToDec =
  foldl'
    (\acc b ->
       case b of
         One -> acc * 2 + 1
         Zero -> acc * 2)
    0

hexCharToBin :: Char -> [Bit]
hexCharToBin c =
  if isAlpha c
    then decToBin (ord c - ord 'A' + 10)
    else decToBin $ read [c]

parsePacket :: [Bit] -> (Packet, [Bit])
parsePacket bits =
  if typeId == 4
    then first (Literal (Version version)) . parseLiteralPayload $ drop 6 bits
    else first (Operator (Version version) typeId) . parseOperator $ drop 6 bits
  where
    version = binToDec $ take 3 bits
    typeId = binToDec . take 3 $ drop 3 bits

parseLiteralPayload :: [Bit] -> (Int, [Bit])
parseLiteralPayload bits = first binToDec $ go bits
  where
    go :: [Bit] -> ([Bit], [Bit])
    go (x:xs) =
      case x of
        One -> first (take 4 xs ++) $ go (drop 4 xs)
        Zero -> splitAt 4 xs
    go [] = error "No more bits :("

parseOperator :: [Bit] -> ([Packet], [Bit])
parseOperator bits =
  let (lengthType, rest) = splitAt 1 bits
   in case head lengthType of
        Zero ->
          let (totalBitLength, rest') = first binToDec $ splitAt 15 rest
           in (parseTotal $ take totalBitLength rest', drop totalBitLength rest')
        One ->
          let (numSubPackets, rest') = first binToDec $ splitAt 11 rest
           in parseNum numSubPackets rest'

parseTotal :: [Bit] -> [Packet]
parseTotal [] = []
parseTotal bits =
  let (firstPacket, rest) = parsePacket bits
   in firstPacket : parseTotal rest

parseNum :: Int -> [Bit] -> ([Packet], [Bit])
parseNum 0 bits = ([], bits)
parseNum n bits =
  let (firstPacket, rest) = parsePacket bits
      (restPackets, rest') = parseNum (n - 1) rest
   in (firstPacket : restPackets, rest')

sumVersions :: Packet -> Int
sumVersions p =
  case p of
    Literal (Version x) _ -> x
    Operator (Version x) _ ps -> x + sum (map sumVersions ps)

sumVersions' :: Packet -> Int
sumVersions' p =
  case p of
    Literal (Version _) v -> v
    Operator (Version _) tId ps ->
      case tId of
        0 -> sum (map sumVersions' ps)
        1 -> product (map sumVersions' ps)
        2 -> minimum (map sumVersions' ps)
        3 -> maximum (map sumVersions' ps)
        5 ->
          let (f:s:_) = ps
           in if sumVersions' f > sumVersions' s
                then 1
                else 0
        6 ->
          let (f:s:_) = ps
           in if sumVersions' f < sumVersions' s
                then 1
                else 0
        7 ->
          let (f:s:_) = ps
           in if sumVersions' f == sumVersions' s
                then 1
                else 0
        other -> error $ "Didn't expect type ID: " ++ show other
