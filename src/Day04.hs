module Day04 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either.Combinators (fromRight')
import Data.List (partition, transpose)
import qualified Data.Set as S
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

newtype Board =
  Board [[Int]]
  deriving (Show)

type Parser = M.Parsec Void String

day4a :: String -> Int
day4a input = calculateScore calledNums firstWinner
  where
    (calledNums, firstWinner) =
      head . uncurry findWinners . getBoards $ input

day4b :: String -> Int
day4b input = calculateScore calledNums lastWinner
  where
    (calledNums, lastWinner) =
      last . uncurry findWinners . getBoards $ input

getBoards :: String -> ([Int], [Board])
getBoards = fromRight' . M.parse parse ""

-- Find the winners, returning them in order along with the numbers that had
-- been called up to that point.
findWinners :: [Int] -> [Board] -> [([Int], Board)]
findWinners bingoNums = go 1 []
  where
    go _ alreadyWon [] = alreadyWon
    go n alreadyWon boards =
      let (winners, notYetWon) =
            partition (hasBoardWon . S.fromList $ take n bingoNums) boards
       in go
            (n + 1)
            (alreadyWon ++ map (\w -> (take n bingoNums, w)) winners)
            notYetWon

-- Check whether or not a board has won BINGO.
hasBoardWon :: S.Set Int -> Board -> Bool
hasBoardWon nums (Board rows) = any complete rcs
  where
    rcs = rows ++ transpose rows -- Rows and columns
    complete = all (`elem` nums)

-- Calculate the score, which is the last number called multiplied by the sum
-- of the numbers in the board which have not yet been called.
calculateScore :: [Int] -> Board -> Int
calculateScore nums (Board rows) = s * last nums
  where
    s = sum $ filter (`notElem` nums) $ concat rows

parse :: Parser ([Int], [Board])
parse = do
  numbers <- L.decimal `M.sepBy` C.char ','
  void (C.string "\n\n")
  boards <- M.some parseBoard
  M.eof
  return (numbers, boards)

parseBoard :: Parser Board
parseBoard = do
  let
    endOrNewline = M.eof <|> void (C.char '\n')
    line =
        M.many (C.char ' ') *> L.decimal `M.sepBy1` M.some (C.char ' ') <*
        endOrNewline
   in do lines <- M.someTill line endOrNewline
         return (Board lines)
