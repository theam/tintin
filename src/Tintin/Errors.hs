module Tintin.Errors
  ( Errors
  , showAndDie
  )
where

import Tintin.Core
require Tintin.Capabilities.Logging

data Errors

showAndDie :: ( Has Logging.Capability eff
              , Show error
              )
           => [error]
           -> Effectful eff ()
showAndDie errors = do
    errors
     |> mapM_ (Logging.err . show)
    error "Errors found. Exiting."

