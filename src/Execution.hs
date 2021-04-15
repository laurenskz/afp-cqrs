{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{-
This module defines how the bussiness logic defined by the user is actually executed. The current mapping is very simple.
We define an intermediate format: Execution. This is essentially a function from any event that the application might
encounter to side effects and possibly new events. These are all described by the execution monad. Like teletype, it can
be actually executed using IO for example. But in theory one could also use a cloud platform for instance. The primary
goal of using an intermediate format like execution is that the application can be decomposed in multiple smaller executions.
If we for example know that two executions will never modify the same state. Or consume the same events. Given the large
amount of information we have about EventHandlers it is possible to create clever execution plans for distributed systems.

Since state is possibly distributed and shared, the execution Monad provides abstractions for locking a certain part of this state.
Later it might be possible to optimize execution when we identify that locking is in certain cases not required.

The currently implemented execution is very simple and demonstrates that it is indeed possible to map an eventhandler
to an execution.


-}
module Execution(executeCompletely) where

import Application
import Control.Monad.Identity

import Data.Data (Proxy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Event
import EventHandler
import TypeUtils

-- A model for describing how an execution should take place
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

-- Basic mapping of eventhandler to execution, locking all resources for the entire execution of the eventhandler
executeEventHandler ::
  i ->
  Subset rs states ->
  Subset ws states ->
  Elem events i ->
  Subset out events ->
  EventHandler rs ws i out a ->
  Execution events states [Event events]
executeEventHandler i rs ws ev out handler = lockAll rs >> executeEventHandler' i rs ws ev out handler <* releaseAll rs

-- Basic execution
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

-- Make function a type but reverse the type parameters
newtype FuncFromTo a b = FuncFromTo {runFunc :: b -> a}
-- Concatenates the result of two functions
concatFuncs :: (Monad m, Monoid a) => FuncFromTo (m a) b -> FuncFromTo (m a) b -> FuncFromTo (m a) b
concatFuncs f1 f2 = FuncFromTo $ \i -> do
  res1 <- runFunc f1 i
  res2 <- runFunc f2 i
  return $ res1 <> res2

-- Creates a function from a specific event to an execution that might fire new events.
eventHandlerExecutionStep ::
  Subset rs states ->
  Subset ws states ->
  Subset out events ->
  Elem events i ->
  EventHandler rs ws i out () ->
  FuncFromTo (Execution events states [Event events]) i
eventHandlerExecutionStep rs ws out ev h = FuncFromTo $ \i -> executeEventHandler i rs ws ev out h

-- Given an Execution for each event, run the correct execution when an event arrives
invoke :: TypedList events (FuncFromTo (Execution events states [Event events])) -> ExecutionStep events states
invoke fs (Ev e s) = runFunc (getValue' e fs) s

type ExecutionStep events states = Event events -> Execution events states [Event events]

-- Creates a single step of the execution, i.e. an event is executed by all eventhandlers. But the resulting events are not executed but collected.
createExecutionStep :: Application hs events states -> TypedList events (FuncFromTo (Execution events states [Event events]))
createExecutionStep (Application HNil events _) = mapTypedList (\_ -> FuncFromTo (const (return []))) events
createExecutionStep (Application (HCons hs rs ws out ev h) evs states) = modifyValue' ev (concatFuncs handler) (createExecutionStep (Application hs evs states))
  where
    handler = eventHandlerExecutionStep rs ws out ev h

-- Creates an executionstep when given an application definition
createApplication :: Application hs events states -> ExecutionStep events states
createApplication = invoke . createExecutionStep

-- Executes all events when given an execution step. The executionstep is repeated for any events that might be generated during execution
executeAll :: [Event events] -> ExecutionStep events states -> Execution events states ()
executeAll [] _ = return ()
executeAll (e : es) f = do
  newEvents <- f e
  executeAll (es ++ newEvents) f

-- Allows one to run an execution in the IO Monad
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

-- Reads the IORef
readStateRef :: IORef a -> IO (Identity a)
readStateRef ref = do
  a <- readIORef ref
  return (Identity a)

--Execute the application in the IO Monad with a list of events and initial states
executeCompletely :: Application hs events states -> [Event events] -> TypedList states Identity -> IO (TypedList states Identity)
executeCompletely app events states = do
  mstates <- traverseTypeList (newIORef . runIdentity) states
  wireExecution mstates (executeAll events (createApplication app))
  traverseTypeList readStateRef mstates
