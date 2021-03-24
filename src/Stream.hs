{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Stream where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Event
import TypeUtils

type EventName = String
type EventPayload = String
type StoredEvent = (EventName, EventPayload)
type Store = [StoredEvent]

data Stream (events :: [Type]) where
  StreamEmpty :: Stream h
  (:+|) :: Event es -> Stream es -> Stream es

infixr 6 :+|

data Parser (events :: [Type]) where
  PNothing :: Parser '[]
  (:<|) :: (StoredEvent -> Maybe e) -> Parser es -> Parser (e : es)

infixr 6 :<|

parseEvent :: Parser es -> StoredEvent -> Maybe (Event es)
parseEvent PNothing _ = Nothing
parseEvent (p :<| ps) s = fmap event (p s) <|> fmap promote (parseEvent ps s)

parseStore :: Parser es -> Store -> Stream es
parseStore p = foldr (f . parseEvent p) StreamEmpty
  where
    f (Just e) s = e :+| s
    f Nothing s = s
