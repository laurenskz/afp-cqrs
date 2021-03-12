{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Poc where

import Control.Monad.State
import Data.Kind (Type)

type UserId = String

type AccountBalance = Int

type NewBalance = Int

newtype ReceivedMoneyEvent = ReceivedMoneyEvent Int

newtype BookedSomethingEvent = BookedSomethingEvent Int

data Handler (events :: [Type]) where
  Empty :: Handler '[]
  (:^|^:) :: (e -> Int) -> Handler es -> Handler (e : es)

infixr 6 :^|^:

class Invokable i a where
  invoke :: i -> a -> Int

instance {-# OVERLAPS #-} Invokable (Handler (e : es)) e where
  invoke (f :^|^: _) = f

instance {-# OVERLAPPABLE #-} (Invokable (Handler es) e') => Invokable (Handler (e : es)) e' where
  invoke (_ :^|^: fs) e = invoke fs e

sampleHandler :: Handler '[ReceivedMoneyEvent, BookedSomethingEvent]
sampleHandler = (\(ReceivedMoneyEvent x) -> x) :^|^: (\(BookedSomethingEvent x) -> x) :^|^: Empty

handleEvent :: Invokable i e => i -> e -> Int
handleEvent = invoke

test :: Int
test = invoke sampleHandler (ReceivedMoneyEvent 5) + invoke sampleHandler (BookedSomethingEvent 5)

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
