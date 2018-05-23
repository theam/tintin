module Tintin.Capabilities.Filesystem
  ( Filesystem
  , Capability
  , Path(..)
  , Extension(..)

  , local
  , deleteIfExists
  , list
  , currentDirectory
  , readFile
  , makeDirectory
  , writeFile
  , doesExist

  , getPathsWith
  )
where

import Tintin.Core hiding (list, local, readFile, writeFile)
import qualified Tintin.Core as Core
import Tintin.Capabilities

require Data.Text


data Filesystem

newtype Path      = Path Text deriving ( Show, Ord, Eq )
newtype Extension = Extension Text


data Capability = Capability
  { _deleteIfExists   :: Path -> IO ()
  , _list             :: Path -> IO [Path]
  , _currentDirectory :: IO Path
  , _readFile         :: Path -> IO Text
  , _makeDirectory    :: Path -> IO ()
  , _writeFile        :: Path -> Text -> IO ()
  , _doesExist        :: Path -> IO Bool
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

  _doesExist (Path p) = doesPathExist (toString p)


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


doesExist :: Has Capability eff
          => Path
          -> Effectful eff Bool
doesExist = liftCapability _doesExist


getPathsWith :: Extension -> [Path] -> [Path]
getPathsWith (Extension e) =
  filter (\(Path fn) -> e `Text.isSuffixOf` fn)
