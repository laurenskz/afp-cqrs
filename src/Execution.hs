{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Execution(executeCompletely) where

import Application
import Control.Monad.Identity

import Data.Data (Proxy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Event
import EventHandler
import TypeUtils

data Execution (events :: [Type]) (states :: [Type]) (a :: Type) where
  -- Functions for manipulating resources
  LockResource :: Elem states s -> Execution events states a -> Execution events states a
  ReleaseResource :: Elem states s -> Execution events states a -> Execution events states a
  ReadResource :: Elem states s -> (s -> Execution events states a) -> Execution events states a
  WriteResource :: Elem states s -> s -> Execution events states a -> Execution events states a
  -- Basic monad
  EReturn :: a -> Execution es ss a

instance Functor (Execution events states) where
  fmap f (EReturn a) = EReturn (f a)
  fmap f (LockResource e s) = LockResource e (fmap f s)
  fmap f (ReleaseResource e s) = ReleaseResource e (fmap f s)
  fmap f (ReadResource e g) = ReadResource e (fmap f . g)
  fmap f (WriteResource e s a) = WriteResource e s (fmap f a)

instance Applicative (Execution events states) where
  pure = EReturn
  EReturn f <*> h = fmap f h
  (LockResource e f) <*> h = LockResource e (f <*> h)
  (ReleaseResource e f) <*> h = ReleaseResource e (f <*> h)
  (ReadResource e f) <*> h = ReadResource e (\res -> f res <*> h)
  (WriteResource e s f) <*> h = WriteResource e s (f <*> h)

instance Monad (Execution events states) where
  EReturn x >>= f = f x
  (LockResource e g) >>= f = LockResource e (g >>= f)
  (ReleaseResource e g) >>= f = ReleaseResource e (g >>= f)
  (ReadResource e g) >>= f = ReadResource e (\res -> g res >>= f)
  (WriteResource e s g) >>= f = WriteResource e s (g >>= f)

lockResource :: Elem states s -> Execution events states ()
lockResource e = LockResource e (EReturn ())

releaseResource :: Elem states s -> Execution events states ()
releaseResource e = ReleaseResource e (EReturn ())

readResource :: Elem states s -> Execution events states s
readResource e = ReadResource e EReturn

writeResource :: Elem states s -> s -> Execution events states ()
writeResource e s = WriteResource e s (EReturn ())

lockAll :: Subset xs states -> Execution events states ()
lockAll TNil = return ()
lockAll (TCons e es) = do
  lockResource e
  lockAll es

releaseAll :: Subset xs states -> Execution events states ()
releaseAll TNil = return ()
releaseAll (TCons e es) = do
  releaseResource e
  releaseAll es

executeEventHandler ::
  i ->
  Subset rs states ->
  Subset ws states ->
  Elem events i ->
  Subset out events ->
  EventHandler rs ws i out a ->
  Execution events states [Event events]
executeEventHandler i rs ws ev out handler = lockAll rs >> executeEventHandler' i rs ws ev out handler <* releaseAll rs

executeEventHandler' ::
  i ->
  Subset rs states ->
  Subset ws states ->
  Elem events i ->
  Subset out events ->
  EventHandler rs ws i out a ->
  Execution events states [Event events]
executeEventHandler' i rs ws ev out (Read e f) = do
  val <- readResource (promoteElement e rs)
  executeEventHandler' i rs ws ev out (f val)
executeEventHandler' i rs ws ev out (Write e v f) = do
  writeResource (promoteElement e ws) v
  executeEventHandler' i rs ws ev out f
executeEventHandler' i rs ws ev out (Handle f) = executeEventHandler' i rs ws ev out (f i)
executeEventHandler' i rs ws ev out (Return _) = return []
executeEventHandler' i rs ws ev out (Raise event f) = do
  events <- executeEventHandler' i rs ws ev out f
  return (promoteSubset out event : events)

newtype FuncFromTo a b = FuncFromTo {runFunc :: b -> a}

eventHandlerExecutionStep ::
  Subset rs states ->
  Subset ws states ->
  Subset out events ->
  Elem events i ->
  EventHandler rs ws i out () ->
  FuncFromTo (Execution events states [Event events]) i
eventHandlerExecutionStep rs ws out ev h = FuncFromTo $ \i -> executeEventHandler i rs ws ev out h

composeFuncs :: (Monad m, Monoid a) => FuncFromTo (m a) b -> FuncFromTo (m a) b -> FuncFromTo (m a) b
composeFuncs f1 f2 = FuncFromTo $ \i -> do
  res1 <- runFunc f1 i
  res2 <- runFunc f2 i
  return $ res1 <> res2

invoke :: TypedList events (FuncFromTo (Execution events states [Event events])) -> ExecutionStep events states
invoke fs (Ev e s) = runFunc (getValue' e fs) s

type ExecutionStep events states = Event events -> Execution events states [Event events]

createExecutionStep :: Application hs events states -> TypedList events (FuncFromTo (Execution events states [Event events]))
createExecutionStep (Application HNil events _) = mapTypedList (\_ -> FuncFromTo (const (return []))) events
createExecutionStep (Application (HCons hs rs ws out ev h) evs states) = modifyValue' ev (composeFuncs handler) (createExecutionStep (Application hs evs states))
  where
    handler = eventHandlerExecutionStep rs ws out ev h

createApplication :: Application hs events states -> ExecutionStep events states
createApplication = invoke . createExecutionStep

executeAll :: [Event events] -> ExecutionStep events states -> Execution events states ()
executeAll [] _ = return ()
executeAll (e : es) f = do
  newEvents <- f e
  executeAll (es ++ newEvents) f

wireExecution :: TypedList states IORef -> Execution event states a -> IO a
wireExecution _ (EReturn x) = return x
wireExecution state (LockResource _ f) = wireExecution state f
wireExecution state (ReleaseResource _ f) = wireExecution state f
wireExecution state (ReadResource e f) = do
  val <- readIORef (getValue' e state)
  wireExecution state (f val)
wireExecution state (WriteResource e w f) = do
  writeIORef (getValue' e state) w
  wireExecution state f

class Executor m where
  execute :: Execution events states a -> m events states a

newtype BasicExecutor (events :: [Type]) (states :: [Type]) a = BasicExecutor {runExecution :: TypedList states IORef -> IO a}

instance Executor BasicExecutor where
  execute e = BasicExecutor (`wireExecution` e)

readStateRef :: IORef a -> IO (Identity a)
readStateRef ref = do
  a <- readIORef ref
  return (Identity a)

executeCompletely :: Application hs events states -> [Event events] -> TypedList states Identity -> IO (TypedList states Identity)
executeCompletely app events states = do
  mstates <- traverseTypeList (newIORef . runIdentity) states
  wireExecution mstates (executeAll events (createApplication app))
  traverseTypeList readStateRef mstates
