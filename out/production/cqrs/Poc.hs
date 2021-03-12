{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Poc where

import Control.Monad.State
import Data.Kind (Type)

-- The identifier of the event is a
-- The payload is b
type TimeStamp = Int

data Command a c = Command TimeStamp a c

type EventId = Int

data Event a e = Event EventId TimeStamp a e

class CommandHandler a a' e c s where
  --  To keep track of side effects we need to know the following:
  --    What identifier did the incoming command have?
  --    State s is assumed to be modified for identifier a
  --    The resulting event can modify all states of the people who consume it
  handle :: Command a c -> State s (Maybe (Event a' e))

type UserId = String

type AccountBalance = Int

type NewBalance = Int

data ReceivedMoneyEvent = ReceivedMoneyEvent Int

data BookedSomethingEvent = BookedSomethingEvent Int

instance CommandHandler UserId UserId NewBalance ReceivedMoneyEvent AccountBalance where
  handle (Command t userId bonus) = do
    modify (+ bonus)
    s <- get
    return (Just (Event 1 t userId s))

data Handler (events :: (e : es)) = Handler (e -> Int) (Handler es) | Empty

type BankAccountHandler = Handler [ReceivedMoneyEvent, BookedSomethingEvent]

sampleHandler :: BankAccountHandler
sampleHandler = Handler (\(ReceivedMoneyEvent e) -> e) 

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
