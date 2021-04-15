{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

{-
This module defines the application of the user. The user has to specify all events and states in the system. 
As well as EventHandlers that alter the state of the system.
These are the bare essentials of an event processing application. But it compromises the essence. In theory the states
can be queried at any time. So also while the system is running. The main idea of this application is that the execution
is completely decoupled from the bussiness logic defined by the user.  
-}
module Application where

import Data.Kind (Type)
import Event
import EventHandler
import TypeUtils

data KHandler = KHandler [Type] [Type] Type [Type]

-- Takes care of putting all handlers in a list
data HandlersT (khandler :: [KHandler]) (events :: [Type]) (states :: [Type]) where
  HNil :: HandlersT '[] events states
  HCons ::
    HandlersT ks events states ->
    Subset rs states ->
    Subset ws states ->
    Subset out events ->
    Elem events i ->
    EventHandler rs ws i out () ->
    HandlersT ( 'KHandler rs ws i out ': ks) events states

addHandler ::
  (Subsettable rs states, Subsettable ws states, Subsettable out events, Indexable events i) =>
  EventHandler rs ws i out () ->
  HandlersT ks events states ->
  HandlersT ( 'KHandler rs ws i out ': ks) events states
addHandler h ht = HCons ht subset subset subset position h

data Application (khandler :: [KHandler]) (events :: [Type]) (states :: [Type]) = Application (HandlersT khandler events states) (TypedList events []) (TypedList states [])
