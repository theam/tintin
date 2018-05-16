module Tintin.Capabilities.Filesystem
  ( Capability
  , Path(..)
  , Extension(..)

  , local
  , deleteIfExists
  , list
  , currentDirectory
  , readFile
  , makeDirectory
  , writeFile
  , getPathsWith
  )
where

import Tintin.Core hiding (list, local, readFile, writeFile)
import qualified Tintin.Core as Core
import Tintin.Capabilities

import qualified Data.Text as Text


newtype Path      = Path Text deriving ( Show, Ord, Eq )
newtype Extension = Extension Text


data Capability = Capability
  { _deleteIfExists   :: Path -> IO ()
  , _list             :: Path -> IO [Path]
  , _currentDirectory :: IO Path
  , _readFile         :: Path -> IO Text
  , _makeDirectory    :: Path -> IO ()
  , _writeFile        :: Path -> Text -> IO ()
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
    return $ sort (Path . toText <$> files)

  _currentDirectory =
    getCurrentDirectory
    |$> toText
    |$> Path

  _readFile (Path p) = Core.readFile (toString p)

  _makeDirectory (Path p) = createDirectoryIfMissing True (toString p)

  _writeFile (Path p) = Core.writeFile (toString p)


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


makeDirectory :: Has Capability eff
              => Path
              -> Effectful eff ()
makeDirectory = liftCapability _makeDirectory


writeFile :: Has Capability eff
          => Path
          -> Text
          -> Effectful eff ()
writeFile = liftCapability _writeFile


getPathsWith :: Extension -> [Path] -> [Path]
getPathsWith (Extension e) =
  filter (\(Path fn) -> e `Text.isSuffixOf` fn)
