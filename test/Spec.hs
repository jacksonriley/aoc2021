{-# LANGUAGE QuasiQuotes #-}

import Day01 (day1a, day1b)
import Day02 (day2a, day2b)
import Day03 (day3a, day3b)
import Day04 (day4a, day4b)
import Day05 (day5a, day5b)
import Day06 (day6a, day6b)
import Day07 (day7a, day7b)
import Day08 (day8a, day8b)
import Day09 (day9a, day9b)
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main =
  hspec $ do
    test1
    test2
    test3
    test4
    test5
    test6
    test7
    test8
    test9

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

test4 =
  let input =
        [r|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|]
   in describe "test4" $ do
        it "a" $ do day4a input `shouldBe` (4512 :: Int)
        it "b" $ do day4b input `shouldBe` (1924 :: Int)

test5 =
  let input =
        [r|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|]
   in describe "test5" $ do
        it "a" $ do day5a input `shouldBe` (5 :: Int)
        it "b" $ do day5b input `shouldBe` (12 :: Int)

test6 =
  let input = "3,4,3,1,2"
   in describe "test6" $ do
        it "a" $ do day6a input `shouldBe` (5934 :: Int)
        it "b" $ do day6b input `shouldBe` (26984457539 :: Int)

test7 =
  let input = "16,1,2,0,4,2,7,1,2,14"
   in describe "test7" $ do
        it "a" $ do day7a input `shouldBe` (37 :: Int)
        it "b" $ do day7b input `shouldBe` (168 :: Int)

test8 =
  let input =
        [r|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|]
   in describe "test8" $ do
        it "a" $ do day8a input `shouldBe` (26 :: Int)
        it "b" $ do day8b input `shouldBe` (61229 :: Int)

test9 =
  let input =
        [r|2199943210
3987894921
9856789892
8767896789
9899965678|]
   in describe "test9" $ do
        it "a" $ do day9a input `shouldBe` (15 :: Int)
        it "b" $ do day9b input `shouldBe` (1134 :: Int)
