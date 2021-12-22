module Day20 where

import Data.Bifunctor (bimap)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Position = (Int, Int)

data Pixel
  = Lit
  | Dim
  deriving (Show, Eq)

day20a :: String -> Int
day20a = countAfterN 2

day20b :: String -> Int
day20b = countAfterN 50

countAfterN :: Int -> String -> Int
countAfterN n input = countLit . snd . (!! n) $ iterate (tick algo) (Dim, m)
  where
    (algo, m) = parse input

readPixel :: Char -> Pixel
readPixel '#' = Lit
readPixel '.' = Dim
readPixel c = error $ "Expected a valid pixel, got " ++ pure c

parse :: String -> ([Pixel], M.Map Position Pixel)
parse input = (algorithm, pixels)
  where
    [algo_s, pixels_s] = splitOn "\n\n" input
    algorithm = map readPixel algo_s
    pixels =
      M.fromList $ do
        (y, line) <- zip [0 ..] $ lines pixels_s
        (x, p) <- zip [0 ..] $ map readPixel line
        return ((x, y), p)

countLit :: M.Map a Pixel -> Int
countLit = length . filter (== Lit) . map snd . M.toList

square :: Position -> [Position]
square (x, y) =
  map
    (bimap (x +) (y +))
    [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

binToDec :: [Pixel] -> Int
binToDec =
  foldl'
    (\acc p ->
       case p of
         Lit -> 2 * acc + 1
         Dim -> 2 * acc)
    0

getIndex :: M.Map Position Pixel -> Pixel -> Position -> Int
getIndex m defaultP = binToDec . map (\p -> fromMaybe defaultP (M.lookup p m)) . square

succP :: Pixel -> Pixel
succP Lit = Dim
succP Dim = Lit

-- Yield the points outside a square with minX == minY and maxX == maxY with a
-- thickness of one.
boundary :: Int -> Int -> [Position]
boundary minX maxX =
  [(x, y) | x <- [(minX - 1) .. (maxX + 1)], y <- beyonds] ++
  [(x, y) | y <- [minX .. maxX], x <- beyonds]
  where
    beyonds = [minX - 1, maxX + 1]

-- Logic is to assume that totally empty squares tick to totally full squares
-- and vice versa.
-- Therefore accept a default pixel which represents the pixels in the rest of
-- the infinite map, and expand the size of the map we calculate each time
-- to ensure that progress into infinity is calculated.
tick :: [Pixel] -> (Pixel, M.Map Position Pixel) -> (Pixel, M.Map Position Pixel)
tick algorithm (defaultP, m) =
  let minX = fst . fst $ M.findMin m
      maxX = fst . fst $ M.findMax m
      expandedMap = foldl' (\acc pos -> M.insert pos defaultP acc) m $ boundary minX maxX
      newMap = M.mapWithKey (\pos _ -> algorithm !! getIndex expandedMap defaultP pos) expandedMap
   in (succP defaultP, newMap)
