{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

{-
Models a stream of events. At the moment not utilized yet. Sources and sinks of events need to be handled somewhere in the execution
process. But our library has not yet fixed this and the choice remains open to the user.
-}
module Stream where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Event
import TypeUtils

type EventName = String
type EventPayload = String
type SerializedEvent = (EventName, EventPayload)
type Store = [SerializedEvent]

data Stream (events :: [Type]) where
  StreamEmpty :: Stream h
  (:+|) :: Event es -> Stream es -> Stream es

infixr 6 :+|

data Parser (events :: [Type]) where
  PNothing :: Parser '[]
  (:<|) :: (SerializedEvent -> Maybe e) -> Parser es -> Parser (e : es)

infixr 6 :<|

parseEvent :: Parser es -> SerializedEvent -> Maybe (Event es)
parseEvent PNothing _ = Nothing
parseEvent (p :<| ps) s = fmap event (p s) <|> fmap promote (parseEvent ps s)

parseStore :: Parser es -> Store -> Stream es
parseStore p = foldr (f . parseEvent p) StreamEmpty
  where
    f (Just e) s = e :+| s
    f Nothing s = s
