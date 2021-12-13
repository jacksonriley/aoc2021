module Day13 where

import Data.List (foldl', intercalate, maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Fold
  = X Int
  | Y Int
  deriving (Show)

type Position = (Int, Int)

day13a :: String -> Int
day13a input = length $ doFold points $ head folds
  where
    (points, folds) = parse input

day13b :: String -> String
day13b input = printPoints $ foldl' doFold points folds
  where
    (points, folds) = parse input

parse :: String -> (S.Set Position, [Fold])
parse input = (parsePositions positions, parseFolds folds)
  where
    (positions:folds:_) = splitOn "\n\n" input

parsePositions :: String -> S.Set Position
parsePositions input =
  S.fromList $ do
    line <- lines input
    let (x:y:_) = read <$> splitOn "," line
    return (x, y)

parseFolds :: String -> [Fold]
parseFolds input = do
  line <- lines input
  let (inst:amount:_) = splitOn "=" line
  let fold =
        if inst == "fold along x"
          then X $ read amount
          else Y $ read amount
  return fold

doFold :: S.Set Position -> Fold -> S.Set Position
doFold points fold = S.map f points
  where
    f (x, y) =
      case fold of
        X foldX ->
          if x > foldX
            then (2 * foldX - x, y)
            else (x, y)
        Y foldY ->
          if y > foldY
            then (x, 2 * foldY - y)
            else (x, y)

printPoints :: S.Set Position -> String
printPoints points =
  let maxX = fst $ maximumBy (\(x, y) (x', y') -> x `compare` x') points
      maxY = snd $ maximumBy (\(x, y) (x', y') -> y `compare` y') points
      line y =
        map
          (\pos ->
             if S.member pos points
               then '#'
               else ' ') $
        zip [0 .. maxX] $ repeat y
   in (:) '\n' $ unlines $ map line [0 .. maxY]
