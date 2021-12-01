{-# LANGUAGE QuasiQuotes #-}

import Day01 (day1a, day1b)
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main =
  hspec $ do
    test1

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
