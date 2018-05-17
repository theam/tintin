module Tintin.Errors
  ( showAndDie
  )
where

import Tintin.Core
import qualified Tintin.Capabilities.Logging as Logging


showAndDie :: ( Has Logging.Capability eff
              , Show error
              )
           => [error]
           -> Effectful eff ()
showAndDie errors = do
    errors
     |> mapM_ (Logging.err . show)
    error "Errors found. Exiting."

