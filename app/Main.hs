module Main where

import Universum

import Options.Generic

import Tintin (OutputDirectory(..), makeLogger, runApp)



data Options = Options
  { outputDirectory :: Maybe Text
  , verbose :: Bool
  }

deriving instance Generic Options
instance ParseRecord Options


main :: IO ()
main = do
  opts <- getRecord "Tintin"
  let outputDir = fromMaybe "dist/tintin/" (outputDirectory opts)
  let isVerbose = verbose opts
  runApp ( makeLogger isVerbose ) ( OutputDirectory outputDir )
