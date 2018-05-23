module Tintin.Capabilities.Process
  ( Process
  , Capability
  , CommandName(..)
  , Arguments(..)
  , StdOut(..)
  , StdErr(..)

  , local
  , read
  )
where

import Tintin.Core hiding (stdout, stderr, local)
import Tintin.Capabilities

import System.Exit (ExitCode(..))


data Process

newtype CommandName = CommandName Text
newtype Arguments   = Arguments [Text]
newtype StdOut      = StdOut Text
newtype StdErr      = StdErr Text

newtype Capability = Capability
  { _read :: CommandName -> Arguments -> IO (Either StdErr StdOut) 
  }


local :: Capability
local =
  Capability {..}
 where
  _read (CommandName cn) (Arguments args) = do
    result <- readProcessWithExitCode (toString cn) (toString <$> args) ""
    case result of
      (ExitSuccess, stdout, _) -> return (Right . StdOut $ toText stdout)
      (_, _, stderr) -> return (Left  . StdErr $ toText stderr)


read :: Has Capability eff
           => CommandName
           -> Arguments
           -> Effectful eff (Either StdErr StdOut)
read = liftCapability _read
