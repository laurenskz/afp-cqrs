{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application where

import Data.Kind (Type)
import Event
import EventHandler
import TypeUtils

data KHandler = KHandler [Type] [Type] Type [Type]

data KEndPoint = KQuery [Type] Type Type

data EndpointsT (kEndpoints :: [KEndPoint]) (states :: [Type]) (responseTypes :: [Type]) where
  ENDNil :: EndpointsT '[] states res

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


data EventSourcesT (sourceTypes :: [Type]) where
  ENil :: EventSourcesT '[]
  EAppend :: [a] -> EventSourcesT as -> EventSourcesT (a ': as)

data Application (khandler :: [KHandler]) (events :: [Type]) (states :: [Type]) = Application (HandlersT khandler events states) (TypedList events []) (TypedList states [])
