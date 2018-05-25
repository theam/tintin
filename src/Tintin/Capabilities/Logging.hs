module Tintin.Capabilities.Logging
  ( Logging
  , Capability
  , log
  , err
  , debug

  , stdOut
  , mute
  )
where

import Tintin.Core
import Tintin.Capabilities

data Logging

{- |
Capability for our context to be able to log from any place where it is available:

@
foo :: Has Logging.Capability e
    => Effectful e ()
foo = do
  log "Something happened :("
  ...
@
-}
data Capability = Capability
  { _log   :: Text -> IO ()
  , _err   :: Text -> IO ()
  , _debug :: Text -> IO ()
  }


-- | Logs a text message using the available logger
log :: Has Capability e
    => Text
    -> Effectful e ()
log = liftCapability _log


-- | Logs an error message using the available logger
err :: Has Capability e
    => Text
    -> Effectful e ()
err = liftCapability _err


-- | Logs a debug message using the available logger
debug :: Has Capability e
      => Text
      -> Effectful e ()
debug = liftCapability _debug


-- | Creates a logger that just prints things to STDOUT
stdOut :: Capability
stdOut = Capability
  { _log   = putTextLn
  , _err   = putTextLn . ("[ERROR] - " <>)
  , _debug = putTextLn . ("[DEBUG] - " <>)
  }


-- | Creates a logger that does literally nothing
mute :: Capability
mute = Capability
  { _log = return . const ()
  , _debug = return . const ()
  , _err   = putTextLn . ("[ERROR] - " <>)
  }
