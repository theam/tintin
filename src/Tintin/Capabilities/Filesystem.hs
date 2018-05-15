module Tintin.Capabilities.Filesystem
  ( Capability
  , Path(..)
  , Extension(..)
  , Filename(..)

  , local
  , deleteIfExists
  , list
  , currentDirectory
  , readFile
  , getFilenamesWith
  )
where

import Tintin.Core hiding (list, local, readFile)
import qualified Tintin.Core as Core
import Tintin.Capabilities

import qualified Data.Text as Text


newtype Path      = Path Text
newtype Extension = Extension Text
newtype Filename  = Filename Text


data Capability = Capability
  { _deleteIfExists   :: Path -> IO ()
  , _list             :: Path -> IO [Path]
  , _currentDirectory :: IO Path
  , _readFile         :: Path -> IO Text
  }


local :: Capability
local =
  Capability {..}
 where
  _deleteIfExists (Path p) = do
    let pathToDelete = toString p
    pathExists <- doesDirectoryExist pathToDelete
    when pathExists (removeDirectoryRecursive pathToDelete)

  _list (Path p) = do
    files <- listDirectory (toString p)
    return (Path . toText . sort <$> files)

  _currentDirectory =
    getCurrentDirectory
    |$> toText
    |$> Path

  _readFile (Path p) = Core.readFile (toString p)


deleteIfExists :: Has Capability eff
               => Path
               -> Effectful eff ()
deleteIfExists = liftCapability _deleteIfExists


list :: Has Capability eff
     => Path
     -> Effectful eff [Path]
list = liftCapability _list


currentDirectory :: Has Capability eff
                 => Effectful eff Path
currentDirectory = liftCapability _currentDirectory


readFile :: Has Capability eff
         => Path
         -> Effectful eff Text
readFile = liftCapability _readFile


getFilenamesWith :: Extension -> [Filename] -> [Filename]
getFilenamesWith (Extension e) =
  filter (\(Filename fn) -> fn `Text.isSuffixOf` e)
