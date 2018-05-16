module Tintin.Capabilities.Logging
  ( Capability
  , log
  , stdOut
  , mute
  )
where

import Tintin.Core
import Tintin.Capabilities


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
newtype Capability = Capability
  { _log :: Text -> IO ()
  }


-- | Logs a text message using the available logger
log :: Has Capability e
    => Text
    -> Effectful e ()
log = liftCapability _log


-- | Creates a logger that just prints things to STDOUT
stdOut :: Capability
stdOut = Capability
  { _log = putTextLn
  }


-- | Creates a logger that does literally nothing
mute :: Capability
mute = Capability
  { _log = return . const ()
  }
