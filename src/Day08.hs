module Day08 where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

day8a :: String -> Int
day8a = length . filter (\x -> length x `elem` [2, 3, 4, 7]) . concatMap snd . parse

day8b :: String -> Int
day8b = sum . map solveLine . parse

parse :: String -> [([S.Set Char], [S.Set Char])]
parse = map parseLine . lines

parseLine :: String -> ([S.Set Char], [S.Set Char])
parseLine l = (map S.fromList signals, map S.fromList digits)
  where
    (signals:digits:_) = map (splitOn " ") $ splitOn " | " l

-- Interpret a list of integers as a decimal number - e.g. [1, 2, 3] -> 123
toDec :: [Int] -> Int
toDec = foldl' f 0
  where
    f acc x = 10 * acc + x

solveLine :: ([S.Set Char], [S.Set Char]) -> Int
solveLine (signals, digits) = toDec $ map (deducedMap M.!) digits
  where
    deducedMap = deduceMapping signals

-- Deduce all of the mappings by process of elimination.
deduceMapping :: [S.Set Char] -> M.Map (S.Set Char) Int
deduceMapping signals =
  M.fromList
    [ (numZero, 0)
    , (numOne, 1)
    , (numTwo, 2)
    , (numThree, 3)
    , (numFour, 4)
    , (numFive, 5)
    , (numSix, 6)
    , (numSeven, 7)
    , (numEight, 8)
    , (numNine, 9)
    ]
  where
    numOne = head . filter ((==) 2 . S.size) $ signals
    numFour = head . filter ((==) 4 . S.size) $ signals
    numSeven = head . filter ((==) 3 . S.size) $ signals
    numEight = head . filter ((==) 7 . S.size) $ signals
    fives = filter ((==) 5 . S.size) signals
    sixes = filter ((==) 6 . S.size) signals
--  numSix is the only one with six characters which does not contain all of
--  the characters for numOne (whereas 0 and 9 both do)
    numSix = head . filter (\s -> not $ numOne `S.isSubsetOf` s) $ sixes
--  The jumbled 'c' is the only one which is disjoint between numSix and numEight
    jumbledC = head . S.toList $ S.difference numEight numSix
--  The jumbled 'f' is the other one in numOne.
    jumbledF = head . S.toList $ S.delete jumbledC numOne
--  This allows us to tell apart 2, 3, and 5:
--   - 2 contains jumbledC but not jumbledF
--   - 3 contains jumbledC and jumbledF
--   - 5 contains jumbledF but not jumbledC
    numTwo = head . filter (\s -> (jumbledC `S.member` s) && not (jumbledF `S.member` s)) $ fives
    numThree = head . filter (\s -> (jumbledC `S.member` s) && (jumbledF `S.member` s)) $ fives
    numFive = head . filter (\s -> not (jumbledC `S.member` s) && (jumbledF `S.member` s)) $ fives
--  This allows us to tell apart 0 and 9 based on the intersection with numFive
    numZero = head . filter (\s -> not $ numFive `S.isSubsetOf` s) $ sixes
    numNine = head . filter (\s -> s /= numSix && s /= numZero) $ sixes
