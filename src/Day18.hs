module Day18 where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (foldl1', partition, tails)

data Number
  = Regular Int
  | Pair Number Number
  deriving (Show)

day18a :: String -> Int
day18a = magnitude . foldl1' (\a b -> reduce $ add a b) . parse

day18b :: String -> Int
day18b = maximum . map maxSum . pairs . parse

parse :: String -> [Number]
parse = map go . lines
  where
    go line =
      let (parsed, []) = parseNumber line
       in parsed

parseNumber :: String -> (Number, String)
parseNumber s =
  if head s == '['
    then let (left, rest) = parseNumber $ tail s
             (right, rest') = parseNumber $ drop 1 rest
          in (Pair left right, tail rest')
    else first (Regular . read) $ span isDigit s

add :: Number -> Number -> Number
add = Pair

fromReg :: Number -> Int
fromReg (Regular n) = n
fromReg x = error "Tried to fromReg on a Pair"

addToLeft :: Int -> Number -> Number
addToLeft x (Regular n) = Regular (x + n)
addToLeft x (Pair l r) = Pair (addToLeft x l) r

addToRight :: Int -> Number -> Number
addToRight x (Regular n) = Regular (x + n)
addToRight x (Pair l r) = Pair l (addToRight x r)

explode :: Int -> Number -> Maybe (Number, Int, Int)
explode depth (Pair l r) =
  if depth == 4
    then Just (Regular 0, fromReg l, fromReg r)
    else case explode (depth + 1) l of
           Just (newL, explL, explR) -> Just (Pair newL (addToLeft explR r), explL, 0)
           Nothing ->
             case explode (depth + 1) r of
               Just (newR, explL, explR) -> Just (Pair (addToRight explL l) newR, 0, explR)
               Nothing -> Nothing
explode _ (Regular _) = Nothing

split :: Number -> Maybe Number
split (Pair l r) =
  case split l of
    Just newL -> Just $ Pair newL r
    Nothing ->
      case split r of
        Just newR -> Just $ Pair l newR
        Nothing -> Nothing
split (Regular n) =
  if n >= 10
    then Just $ Pair (Regular $ n `div` 2) (Regular $ (n + 1) `div` 2)
    else Nothing

reduce :: Number -> Number
reduce n =
  case explode 0 n of
    Just (n', _, _) -> reduce n'
    Nothing -> maybe n reduce (split n)

magnitude :: Number -> Int
magnitude (Regular n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

maxSum :: (Number, Number) -> Int
maxSum (a, b) = max (go a b) (go b a)
  where
    go x y = magnitude . reduce $ add x y
