module Tintin.Core
  ( module Exported
  , Logger
  , makeLogger
  , deleteOutputDirectoryIfExists
  , changeExtensionTo
  )
where

import Universum as Exported

import qualified Data.Text as Text
import System.Directory as Exported
import System.IO.Temp as Exported
import System.Process as Exported

import Tintin.Domain as Exported


type Logger = (Text -> IO ())

makeLogger :: Bool -> Logger
makeLogger shouldLog msg =
  when shouldLog (putStrLn msg)


deleteOutputDirectoryIfExists :: OutputDirectory -> IO ()
deleteOutputDirectoryIfExists (OutputDirectory outputDir) = do
  outputDirExists <- doesDirectoryExist ( toString outputDir )
  when outputDirExists (removeDirectoryRecursive $ toString outputDir)


changeExtensionTo :: Text -> Text -> Text
changeExtensionTo filename newExtension =
  fst ( Text.breakOn "." filename ) <> newExtension

