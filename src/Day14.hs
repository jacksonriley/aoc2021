module Day14 where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Pair = (Char, Char)

day14a :: String -> Int
day14a input = last freqs - head freqs
  where
    (template, rules) = parse input
    polymer = (!! 10) $ iterate (insert rules) template
    freqs = sort . map length . group . sort $ polymer

day14b :: String -> Int
day14b = undefined

parse :: String -> (String, M.Map Pair Char)
parse input = (template, insertions)
  where
    [template, insertionRules] = splitOn "\n\n" input
    insertions =
      M.fromList $ do
        line <- lines insertionRules
        let [[left, right], [to]] = splitOn " -> " line
        return ((left, right), to)

insert :: M.Map Pair Char -> String -> String
insert m (left:right:xs) =
  case M.lookup (left, right) m of
    Just c -> left : c : insert m (right : xs)
    Nothing -> left : insert m (right : xs)
insert m xs = xs
