{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Poc where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Kind (Type)
import Data.Typeable

type UserId = String

type AccountBalance = Int

type NewBalance = Int

newtype ReceivedMoneyEvent = ReceivedMoneyEvent Int

instance Parsable ReceivedMoneyEvent where
  parse "" = ReceivedMoneyEvent 0

newtype BookedSomethingEvent = BookedSomethingEvent Int

instance Parsable BookedSomethingEvent where
  parse "" = BookedSomethingEvent 0

class Parsable a where
  parse :: String -> a

instance (Parsable a) => Parsable (a -> Int) where
  parse = parse

data Handler (events :: [Type]) where
  Empty :: Handler '[]
  (:^|) :: (Typeable e, Parsable e) => (e -> Int) -> Handler es -> Handler (e : es)

infixr 6 :^|

data Elem xs x where
  Here :: Elem (x ': xs) x
  There :: Elem xs x -> Elem (y ': xs) x

invoke :: Handler es -> Event es -> Int
invoke Empty (Ev _ _) = 0
invoke (f :^| _) (Ev Here e) = f e
invoke (_ :^| fs) (Ev (There s) e) = invoke fs (Ev s e)

class Indexable xs x where
  position :: Elem xs x

instance {-# OVERLAPS #-} Indexable (e : es) e where
  position = Here

instance {-# OVERLAPS #-} (Indexable es e) => Indexable (e' : es) e where
  position = There position

sampleHandler :: Handler '[ReceivedMoneyEvent, BookedSomethingEvent]
sampleHandler = received :^| booked :^| Empty
  where
    received (ReceivedMoneyEvent x) = x
    booked (BookedSomethingEvent x) = x

test :: Int
test = invoke sampleHandler (event (ReceivedMoneyEvent 5)) + invoke sampleHandler (event (BookedSomethingEvent 5))

test2 = invoke sampleHandler (event (ReceivedMoneyEvent 5))

test3 = invoke sampleHandler (event (BookedSomethingEvent 5))

newtype UnhandledEvent = UnhandledEvent Int

data Event (as :: [Type]) where
  Ev :: Elem as a -> a -> Event as

data All f xs where
  Nil :: All f '[]
  Cons :: f x -> All f xs -> All f (x ': xs)

type IsSubset xs ys = All (Elem ys) xs

event :: (Indexable as a) => a -> Event as
event = Ev position

promote :: Event as -> Event (a : as)
promote (Ev p e) = Ev (There p) e

data Stream (events :: [Type]) where
  StreamEmpty :: Stream h
  (:+|) :: Event es -> Stream es -> Stream es

infixr 6 :+|

data Parser (events :: [Type]) where
  PNothing :: Parser '[]
  (:<|) :: (String -> Maybe e) -> Parser es -> Parser (e : es)

infixr 6 :<|

parseEvent :: Parser es -> String -> Maybe (Event es)
parseEvent PNothing _ = Nothing
parseEvent (p :<| ps) s = fmap event (p s) <|> fmap promote (parseEvent ps s)

events :: Stream '[ReceivedMoneyEvent, BookedSomethingEvent]
events = event (ReceivedMoneyEvent 0) :+| event (BookedSomethingEvent 0) :+| event (BookedSomethingEvent 0) :+| StreamEmpty

process :: Handler ts -> Stream ts -> Int
process h (e :+| es) = invoke h e + process h es
process _ StreamEmpty = 0

parser :: Parser '[ReceivedMoneyEvent, BookedSomethingEvent]
parser =
  (\c -> if c == "ReceivedMoneyEvent" then Just (ReceivedMoneyEvent 6) else Nothing)
    :<| (\c -> if c == "BookedSomethingEvent" then Just (BookedSomethingEvent 6) else Nothing)
    :<| PNothing

store :: [String]
store = replicate 2 "BookedSomethingEvent" ++ replicate 5 "ReceivedMoneyEvent"

parseStore :: Parser es -> [String] -> Stream es
parseStore p = foldr (f . parseEvent p) StreamEmpty
  where
    f (Just e) s = e :+| s
    f Nothing s = s

bootstrap :: Handler es -> Parser es -> Int
bootstrap h p = process h (parseStore p store)

poc :: Int
poc = bootstrap sampleHandler parser

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
