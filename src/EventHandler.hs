{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module EventHandler (EventHandler (), raiseEvent, handle, readState, writeState, runHandler,EventResult) where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Event
import TypeUtils

{-
readStates : The states which the handler may require in order to fulfill the processing of the event
writeStates: The states which may be modified
inEvent: The event being handled
outEvents: Events which may be fired
a : The monad type variable
-}
data EventHandler (readStates :: [Type]) (writeStates :: [Type]) (inEvent :: Type) (outEvents :: [Type]) (a :: Type) where
  Return :: a -> EventHandler r w i out a
  Read :: Elem rs r -> (r -> EventHandler rs w i out a) -> EventHandler rs w i out a
  Write :: Elem ws w -> w -> EventHandler rs ws i out a -> EventHandler rs ws i out a
  Raise :: Event out -> EventHandler rs ws i out a -> EventHandler rs ws i out a
  Handle :: (i -> EventHandler rs ws i out a) -> EventHandler rs ws i out a

instance Functor (EventHandler rs ws i out) where
  fmap f (Return a) = Return (f a)
  fmap f (Read e f') = Read e (fmap f . f')
  fmap f (Write e w h) = Write e w (fmap f h)
  fmap f (Raise out h) = Raise out (fmap f h)
  fmap f (Handle h) = Handle (fmap f . h)

instance Applicative (EventHandler rs ws i out) where
  pure = Return
  Return f <*> h = fmap f h
  Read e f <*> h = Read e (\x -> f x <*> h)
  Write e w f <*> h = Write e w (f <*> h)
  Raise out f <*> h = Raise out (f <*> h)
  Handle f <*> h = Handle (\x -> f x <*> h)

instance Monad (EventHandler rs ws i out) where
  Return x >>= f = f x
  Read e f' >>= f = Read e (\x -> f' x >>= f)
  Write e w x >>= f = Write e w (x >>= f)
  Raise out x >>= f = Raise out (x >>= f)
  Handle f' >>= f = Handle (\x -> f' x >>= f)

readState :: (Indexable rs r) => EventHandler rs ws i out r
readState = Read position return

writeState :: (Indexable ws w) => w -> EventHandler rs ws i out ()
writeState w = Write position w (return ())

raiseEvent :: (Indexable out a) => a -> EventHandler rs ws i out ()
raiseEvent out = Raise (event out) (return ())

handle :: EventHandler rs ws i out i
handle = Handle return

data EventResult out states = EventResult [Event out] (TypedList states Maybe)

instance (Show (Event out), Show (TypedList states Maybe)) => Show (EventResult out states) where
  show (EventResult out states) = "EventResult:\n\t-Events:" ++ show out ++ "\n\t-States:" ++ show states

runHandler :: Empty ws => i -> TypedList rs IO -> EventHandler rs ws i out a -> IO (EventResult out ws)
runHandler _ _ (Return _) = return (EventResult [] (emptyTypeList Nothing))
runHandler ev iors (Raise out xs) = do
  (EventResult outs ws) <- runHandler ev iors xs
  return (EventResult (out : outs) ws)
runHandler ev iors (Write pos w xs) = do
  (EventResult outs ws) <- runHandler ev iors xs
  let updated = modifyValue' pos (<|> Just w) ws
  return (EventResult outs updated)
runHandler ev iors (Read pos f) = getValue' pos iors >>= runHandler ev iors . f
runHandler ev iors (Handle f) = runHandler ev iors (f ev)
