module Day14 where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Pair = (Char, Char)

day14a :: String -> Int
day14a = getDiff 10

day14b :: String -> Int
day14b = getDiff 40

parse :: String -> (String, M.Map Pair Char)
parse input = (template, insertions)
  where
    [template, insertionRules] = splitOn "\n\n" input
    insertions =
      M.fromList $ do
        line <- lines insertionRules
        let [[left, right], [to]] = splitOn " -> " line
        return ((left, right), to)

insert :: M.Map Pair Char -> M.Map Pair Int -> M.Map Pair Int
insert pairs = M.fromListWith (+) . concatMap doReplacement . M.toList
  where
    doReplacement ((left, right), count) =
      case M.lookup (left, right) pairs of
        Just c -> [((left, c), count), ((c, right), count)]
        Nothing -> [((left, right), count)]

countChars :: M.Map Pair Int -> M.Map Char Int
countChars =
  M.fromListWith (+) .
  concatMap (\((left, right), count) -> [(left, count), (right, count)]) . M.toList

-- Get the difference between the most common and least common element after
-- carrying out `n` steps.
-- Do this by keeping track of the numbers of each type of pair - very similar
-- approach to the lanternfish challenge (day 6).
getDiff :: Int -> String -> Int
getDiff n input = (last freqs - head freqs) `div` 2
  where
    (template, rules) = parse input
    -- Construct a map of all pairs in the template to the number of occurences
    -- of each pair - this is what we will iteratively expand.
    templatePairs = M.fromListWith (+) $ zip (zip template $ tail template) (repeat 1)
    polymer = (!! n) $ iterate (insert rules) templatePairs
    charOccurences = countChars polymer
    -- Need to add the ends back in as they won't have been counted twice,
    -- unlike all other characters in the middle of the polymer.
    charOccurencesWithEnds =
      M.insertWith (+) (head template) 1 $ M.insertWith (+) (last template) 1 charOccurences
    freqs = sort $ M.elems charOccurencesWithEnds
