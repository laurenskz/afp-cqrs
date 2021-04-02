{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Poc where

import Application
import Control.Applicative ((<|>))
import Control.Monad.Identity
import Control.Monad.Reader (MonadReader, Reader, ReaderT)
import qualified Control.Monad.Reader as Mread
import Control.Monad.State
import Data.Kind (Type)
import Data.Typeable
import Event
import EventHandler
import Execution
import Stream
import TypeUtils

type UserId = String

type AccountBalance = Int

type NewBalance = Int

newtype BookedSomethingEvent = BookedSomethingEvent Int deriving (Show)

newtype ReceivedMoneyEvent = ReceivedMoneyEvent Int deriving (Show)

newtype WonSomethingEvent = WonSomethingEvent String deriving (Show)

data Handler (events :: [Type]) s where
  Empty :: Handler '[] s
  (:^|) :: (Parsable e, Typeable (e -> State s ())) => (e -> State s ()) -> Handler es s -> Handler (e : es) s

infixr 6 :^|

invoke :: Handler es s -> Event es -> State s ()
invoke Empty (Ev _ _) = return ()
invoke (f :^| _) (Ev Here e) = f e
invoke (_ :^| fs) (Ev (There s) e) = invoke fs (Ev s e)

sampleHandler :: Handler '[ReceivedMoneyEvent, BookedSomethingEvent] Int
sampleHandler = received :^| booked :^| Empty
  where
    received (ReceivedMoneyEvent x) = modify (x +)
    booked (BookedSomethingEvent x) = modify (\s -> s - x)

process :: Handler es s -> Stream es -> State s ()
process h (e :+| es) = invoke h e >> process h es
process _ StreamEmpty = return ()

class Parsable a where
  parse :: String -> a

instance Parsable ReceivedMoneyEvent where
  parse = ReceivedMoneyEvent . read

instance Parsable BookedSomethingEvent where
  parse = BookedSomethingEvent . read

eventName :: (Typeable a) => a -> String
eventName = show . head . typeRepArgs . typeOf

mkparser :: Handler es s -> Parser es
mkparser Empty = PNothing
mkparser (h :^| hs) = make' :<| mkparser hs
  where
    make' (n, p)
      | n == eventName h = Just $ parse p
      | otherwise = Nothing

store :: Store
store = replicate 2 ("BookedSomethingEvent", "6") ++ replicate 5 ("ReceivedMoneyEvent", "6")

bootstrap :: Handler es s -> Store -> s -> s
bootstrap h = execState . process h . parseStore (mkparser h)

poc :: Int
poc = bootstrap sampleHandler store 0

sampleEventHandler :: EventHandler '[Int, String] '[Int, Bool] ReceivedMoneyEvent '[BookedSomethingEvent, WonSomethingEvent] ()
sampleEventHandler = do
  currentInt :: Int <- readState
  currentString :: String <- readState
  writeState (currentInt < 6)
  writeState (6 * currentInt)
  raiseEvent (BookedSomethingEvent 5)
  raiseEvent (WonSomethingEvent currentString)

testSampleHandler :: IO (EventResult '[BookedSomethingEvent, WonSomethingEvent] '[Int, Bool])
testSampleHandler = runHandler (ReceivedMoneyEvent 4) (TCons (return 4) (TCons (return "Hello world!") TNil)) sampleEventHandler

hank = emptyTypeList Nothing :: TypedList [Bool, Int] Maybe

henk :: Event '[String, Bool]
henk = event True

newtype Balance = Balance Int deriving (Show)

newtype Price = Price String deriving (Show)

newtype User = User String deriving (Show)

type POCEvents = '[BookedSomethingEvent, WonSomethingEvent, ReceivedMoneyEvent]

type POCStates = '[Balance, Price, User]

emptyHandlers :: HandlersT '[] POCEvents POCStates
emptyHandlers = HNil

pocHandler1 :: EventHandler '[Balance] '[Price, Balance] ReceivedMoneyEvent '[WonSomethingEvent] ()
pocHandler1 = do
  (ReceivedMoneyEvent x) <- handle
  (Balance current) <- readState
  writeState (Balance (current + x))
  raiseEvent (WonSomethingEvent ("Price! being added to balance of " ++ show current))

pocHandler2 :: EventHandler '[Price] '[User] WonSomethingEvent '[] ()
pocHandler2 = do
  (WonSomethingEvent name) <- handle
  writeState (User ("User with a nice price!:" ++ name))

type POCHandlers =
  '[ 'KHandler '[Balance] '[Price, Balance] ReceivedMoneyEvent '[WonSomethingEvent],
     'KHandler '[Price] '[User] WonSomethingEvent '[]
   ]

handlers :: HandlersT POCHandlers POCEvents POCStates
handlers = addHandler pocHandler1 (addHandler pocHandler2 emptyHandlers)

initialStates :: TypedList POCStates Identity
initialStates = TCons (Identity $ Balance 0) (TCons (Identity $ Price "") (TCons (Identity $ User "") TNil))

application :: Application POCHandlers POCEvents POCStates
application = Application handlers (emptyTypeList []) (emptyTypeList [])

pocEvents :: [Event POCEvents]
pocEvents = [event $ ReceivedMoneyEvent 3]

runPoc :: IO ()
runPoc = do
  finalStates <- executeCompletely application pocEvents initialStates
  print finalStates

{-
Some key observations:
  - The order of events is important, we have to make sure they are handled in the exact same order to guarantee reproducibility
  - Commands cannot modify the current state
  - The only valid source of new events is an owner of the state paired with identifier
  - The only way to update the owner of a state is through an event
  - Events have all information to identify all owners by which they are handled
  - An owner is said to be valid for event e if all events and commands lower in the order have been handled.

  What happens when two owners have an overlapping state?
  Suppose one owner produces an event that causes the following event:
    - AccountBalance 10
    - AccountBalance 100
  The event is actually some kind of state in this case, this can happen because the owner is actually not the owner but there is overlap

  What we offer:
    - The guarantee that the events are processed in such a way that the owner only accepts events if it is in a valid state.
      => Very strong:
        - Traceability
        - Consistency
-}
