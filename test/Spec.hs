{-# LANGUAGE QuasiQuotes #-}

import Day01 (day1a, day1b)
import Day02 (day2a, day2b)
import Day03 (day3a, day3b)
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main =
  hspec $ do
    test1
    test2
    test3

test1 =
  let input =
        [r|199
200
208
210
200
207
240
269
260
263|]
   in describe "test1" $ do
        it "a" $ do day1a input `shouldBe` (7 :: Int)
        it "b" $ do day1b input `shouldBe` (5 :: Int)

test2 =
  let input =
        [r|forward 5
down 5
forward 8
up 3
down 8
forward 2|]
   in describe "test2" $ do
        it "a" $ do day2a input `shouldBe` (150 :: Int)
        it "b" $ do day2b input `shouldBe` (900 :: Int)

test3 =
  let input =
        [r|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|]
   in describe "test3" $ do
        it "a" $ do day3a input `shouldBe` (198 :: Int)
        it "b" $ do day3b input `shouldBe` (230 :: Int)
