module Tintin.Core
  ( module Exported

  , Effectful
  , Pure

  , runEffects
  , flatMap
  )
where

import Universum as Exported hiding (log)

import Data.Has as Exported
import System.Directory as Exported
import System.IO.Temp as Exported
import System.Process as Exported

import Tintin.Domain as Exported



type Effectful context result =
  ReaderT context IO result

runEffects :: Effectful context result
           -> context
           -> IO result
runEffects = runReaderT

type Pure context result =
  Reader context result


-- (|>) :: a -> (a -> b) -> b
-- (|>) = (&)
--
-- (|$>) :: Functor f => f a -> (a -> b) -> f b
-- (|$>) = (<&>)
--
-- (|>>) :: Monad m => m a -> (a -> m b) -> m b
-- (|>>) = (>>=)

flatMap :: Monad m => (a -> m b) -> m a -> m b
flatMap = (=<<)
