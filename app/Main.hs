module Main where

import Lib

main :: IO ()
main = do
  print (rndNumbers 5 100)
