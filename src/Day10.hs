module Day10 where

import Data.List (elemIndex, foldl', sort)
import Data.Maybe (mapMaybe)

newtype BracketStack =
  BracketStack String
  deriving (Show)

data LineType
  = Corrupted Char
  | Incomplete String

day10a :: String -> Int
day10a = sum . map scoreIllegal . mapMaybe (getCorrupted . classifyLine) . lines
  where
    getCorrupted (Corrupted c) = Just c
    getCorrupted (Incomplete _) = Nothing

day10b :: String -> Int
day10b input = scores !! scoresMid
  where
    scores = sort . map scoreIncomplete . mapMaybe (getIncomplete . classifyLine) . lines $ input
    scoresMid = length scores `div` 2
    getIncomplete (Corrupted _) = Nothing
    getIncomplete (Incomplete s) = Just s

-- Classify a given line as being either corrupted or incomplete.
classifyLine :: String -> LineType
classifyLine = go (BracketStack [])
  where
    go stack (x:xs) =
      case consider x stack of
        Right stack' -> go stack' xs
        Left c -> Corrupted c
    go (BracketStack stack) [] = Incomplete stack

-- Consider a given bracket in the context of a stack of previous brackets. If
-- the character is legal (either it is an opening bracket or its pair is on
-- top of the stack), return Right of the new stack. Otherwise, return Left of
-- the character.
consider :: Char -> BracketStack -> Either Char BracketStack
consider c (BracketStack stack@(top:rest))
  | isOpen c = Right $ BracketStack $ c : stack
  | top == getPair c = Right $ BracketStack rest
  | otherwise = Left c
consider c (BracketStack [])
  | isOpen c = Right $ BracketStack [c]
  | otherwise = Left c

-- Score the illegal characters as specified.
scoreIllegal :: Char -> Int
scoreIllegal c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = error $ "Unexpected illegal character to score: " ++ [c]

-- Score the characters required to make an incomplete line complete (but
-- flipped because who cares).
scoreIncomplete :: String -> Int
scoreIncomplete = foldl' go 0
  where
    go acc c = acc * 5 + scoreChar c
    scoreChar c
      | c == '(' = 1
      | c == '[' = 2
      | c == '{' = 3
      | c == '<' = 4
      | otherwise = error $ "Unexpected character in the incomplete list: " ++ [c]

-- Is a character an opening bracket
isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

-- Get the pair of any bracket
getPair :: Char -> Char
getPair c = pair
  where
    all = "([{<)]}>"
    n =
      case elemIndex c all of
        Just x -> x
        Nothing -> error $ "Unexpected char: " ++ [c]
    pair = all !! ((n + 4) `mod` 8)
