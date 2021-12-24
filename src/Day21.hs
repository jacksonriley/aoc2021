module Day21 where

import Data.List (foldl', group, sort)
import qualified Data.Map.Strict as M
import Debug.Trace

data Player
  = P1
  | P2
  deriving (Show, Ord, Eq)

data GameState =
  GameState
    { positions :: (Int, Int)
    , scores :: (Int, Int)
    , diceState :: Int
    , rollNumber :: Int
    , playerTurn :: Player
    }
  deriving (Show)

day21a :: String -> Int
day21a = playUntilWinA . parse

parse :: String -> (Int, Int)
parse input = (p1, p2)
  where
    p1 = read . pure . last . head $ lines input
    p2 = read . pure . last . last $ lines input

wrapN :: Int -> Int -> Int
wrapN m n = 1 + ((n - 1) `mod` m)

rollDeterministic :: Int -> Int -> Int
rollDeterministic diceState position = newPos
  where
    newPos = wrapN 10 . (position +) . sum . map (wrapN 100) $ take 3 [diceState ..]

succP :: Player -> Player
succP P1 = P2
succP P2 = P1

turnA :: GameState -> GameState
turnA GameState { positions = (p1, p2)
                , scores = (s1, s2)
                , diceState = diceState
                , rollNumber = rollNumber
                , playerTurn = playerTurn
                } = GameState positions' scores' (diceState + 3) (rollNumber + 3) (succP playerTurn)
  where
    (positions', scores') =
      case playerTurn of
        P1 ->
          let newp1Pos = rollDeterministic diceState p1
           in ((newp1Pos, p2), (s1 + newp1Pos, s2))
        P2 ->
          let newp2Pos = rollDeterministic diceState p2
           in ((p1, newp2Pos), (s1, s2 + newp2Pos))

gameIsWonA :: GameState -> Maybe (Int, Int)
gameIsWonA GameState { positions = (p1, p2)
                     , scores = (s1, s2)
                     , diceState = diceState
                     , rollNumber = rollNumber
                     , playerTurn = playerTurn
                     }
  | s1 >= 1000 = Just (s2, rollNumber)
  | s2 >= 1000 = Just (s1, rollNumber)
  | otherwise = Nothing

playUntilWinA :: (Int, Int) -> Int
playUntilWinA (p1, p2) = losing * turns
  where
    (losing, turns) = go $ GameState (p1, p2) (0, 0) 1 0 P1
    go :: GameState -> (Int, Int)
    go gs =
      case gameIsWonA gs of
        Just x -> x
        Nothing -> go $ turnA gs

-- Part two
day21b :: String -> Int
day21b input =
  if w1 > w2
    then w1
    else w2
  where
    (p1, p2) = parse input
    (_, w1, w2) = rollDiracDice (p1, p2, 0, 0, P1) M.empty

-- p1, p2, s1, s2, turn
type GameState' = (Int, Int, Int, Int, Player)

type DP = M.Map GameState' (Int, Int)

-- All possible totals from rolling a dirac die three times, along with their
-- counts.
sum3 :: [(Int, Int)]
sum3 =
  map (\l -> (length l, head l)) . group . sort $
  [a + b + c | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]

updateState :: GameState' -> Int -> GameState'
updateState (p1, p2, s1, s2, P1) rollTotal = (p1', p2, s1', s2, P2)
  where
    p1' = wrapN 10 $ p1 + rollTotal
    s1' = s1 + p1'
updateState (p1, p2, s1, s2, P2) rollTotal = (p1, p2', s1, s2', P1)
  where
    p2' = wrapN 10 $ p2 + rollTotal
    s2' = s2 + p2'

rollDiracDice :: GameState' -> DP -> (DP, Int, Int)
rollDiracDice gs@(p1, p2, s1, s2, turn) m
  | s1 >= 21 = (m, 1, 0)
  | s2 >= 21 = (m, 0, 1)
  | otherwise =
    case M.lookup gs m of
      Just (w1, w2) -> (m, w1, w2)
      Nothing ->
        let (m', w1, w2) = foldl' go (m, 0, 0) sum3
         in (M.insert gs (w1, w2) m', w1, w2)
        where go :: (DP, Int, Int) -> (Int, Int) -> (DP, Int, Int)
              go (m', w1, w2) (numTimes, rollValue) =
                let (m'', w1', w2') = rollDiracDice (updateState gs rollValue) m'
                 in (m'', w1 + numTimes * w1', w2 + numTimes * w2')
