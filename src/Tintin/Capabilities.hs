
module Tintin.Capabilities
  (
    {-|
'liftCapability' allows executing an 'IO' action stored in
a Capability value in the 'Effectful' monad.

It is intended to be used within the Capability definition
module, not outside.

@
data MyCapability = MyCapability
  { _myAction :: IO ()
  }

myAction :: Has MyCapability e
         => Effectful e ()
myAction = liftCapability _myAction
@
    -}
    liftCapability
  )
where

import Tintin.Core


{-
This class should NOT be instantiated outside of this module,
as it serves to make the code more clean outside.

It allows us to do a

liftCapability f

instead of

liftCapability $ \capability -> f capability foo bar quux

where foo, bar, and quux are the remaining arguments.

These two instances allow the compiler to recurse on the arguments
and typecheck that the application is correct.

See: https://stackoverflow.com/q/50353294/3847023
-}

class LiftableWithCapability v where
  -- v must have an execution type like 'IO a'
  type Execution v -- :: *

  -- also, v must have a context available, to get the Capability from
  type Context v -- :: *

  -- We provide liftCapability, that ensures that it exists in the Context of v
  -- and, after passing a function that takes the capability and executes it in the
  -- Execution type of V
  liftCapability :: Has capability (Context v) => (capability -> Execution v) -> v



-- We instantiate it for our Effectful type, which is an alias for ReaderT e IO a
instance LiftableWithCapability (ReaderT e IO a) where

  -- The execution type of that is 'IO a'
  type Execution (ReaderT e IO a) = IO a

  -- And the context is e
  type Context (ReaderT e IO a) = e

  -- liftCapability just grabs the capability and calls the function on it
  liftCapability f = do
    capability <- asks getter
    lift ( f capability )


-- a function that takes some parameter a and returns v also can be lifter
instance LiftableWithCapability v => LiftableWithCapability (a -> v) where

  -- The execution type of the function, is just the Execution of the return type
  type Execution (a -> v) = a -> Execution v

  -- Same with the Context, the Context of the return type
  type Context (a -> v) = Context v

  -- We flip the arguments of f, so the capability record stays in the last place
  -- and we can recurse over it, until we reach the definition from before.
  --
  -- It is the same as doing 'liftCapability $ \x -> f x b'
  liftCapability f = liftCapability . flip f

