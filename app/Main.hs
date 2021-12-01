module Main where

import Data.Maybe
import Lib (libmain)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  libmain $ read <$> listToMaybe args
