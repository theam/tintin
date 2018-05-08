module Main where

import Options.Generic

import Tintin

main :: IO ()
main = do
  opts <- getRecord "Tintin"
  runApp opts
