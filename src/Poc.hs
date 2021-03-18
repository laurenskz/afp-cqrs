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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Poc where

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

-- instance (Show e) => Show (e -> Int) where
--   show f = show $ typeRep f

data Test = Not | First | Succ Test
type family Member' x xs where
   Member' x '[] = Not
   Member' x (x ': xs) = First
   Member' x (y ': xs) = Succ (Member' x xs)


data Elem xs x where
  Here :: Elem (x ': xs) x
  There :: Elem xs x -> Elem (y ': xs) x
  
invoke' :: Elem es e -> Handler es -> e -> Int
invoke' Here (f :^| _) = f
invoke' (There s) (_ :^| fs) = invoke' s fs
invoke :: (Member es e) => Handler es -> e -> Int
invoke = invoke' position

class Member xs x where
  position :: Elem xs x
instance {-# OVERLAPS #-} Member (e : es) e where
  position = Here
instance {-# OVERLAPS #-} (Member es e) => Member (e' : es) e where
  position = There position

sampleHandler :: Handler '[ReceivedMoneyEvent, BookedSomethingEvent]
sampleHandler = received :^| booked :^| Empty
  where
    received (ReceivedMoneyEvent x)   = x
    booked   (BookedSomethingEvent x) = x

test :: Int
test = invoke sampleHandler (ReceivedMoneyEvent 5) + invoke sampleHandler (BookedSomethingEvent 5)
test2 = invoke sampleHandler (ReceivedMoneyEvent 5)
test3 = invoke sampleHandler (BookedSomethingEvent 5)

newtype UnhandledEvent = UnhandledEvent Int

data Stream h where
  StreamEmpty :: Stream h
  (:+|) :: (Member es e) => e -> Stream es -> Stream es

infixr 6 :+|

events :: Stream '[ReceivedMoneyEvent, BookedSomethingEvent]
events = ReceivedMoneyEvent 0 :+| BookedSomethingEvent 0 :+| BookedSomethingEvent 0 :+| StreamEmpty

process :: Handler ts -> Stream ts -> Int
process h (e :+| es) = invoke h e + process h es

eventName :: (Typeable a) => a -> String
eventName = show . head . typeRepArgs . typeOf

eventNames :: Handler es -> [String]
eventNames (e :^| es) = eventName e : eventNames es
eventNames Empty      = []


parse' :: Elem es e -> Handler es -> String -> e
parse' Here (f :^| _) = parse
parse' (There s) (_ :^| fs) = parse' s fs

parseEvent :: Handler es -> String -> e
parseEvent h json = parse' (parseEvent' h json) h json

parseEvent' :: Handler es -> String -> Elem es e
parseEvent' (e :^| es) json 
  | json == eventName e = undefined -- TODO Here
  | otherwise = There $ parseEvent' es json

-- t = process sampleHandler $ x :+| StreamEmpty

-- getStream h = foldr c StreamEmpty names -- TODO read from a store
-- getStream h = foldr (\n p -> parseEvent h n :+| p) g names
getStream :: Handler es -> e
getStream h = parseEvent h $ head $ eventNames h
  -- where
  --   names  = eventNames h
  --   c :: String -> Stream ts -> Stream ts
  --   -- c "ReceivedMoneyEvent"   p = ReceivedMoneyEvent   0 :+| p
  --   -- c "BookedSomethingEvent" p = BookedSomethingEvent 0 :+| p
  --   c e _ = error $ "Unexpected event: " ++ e

-- bootstrap :: Handler es -> Int
bootstrap h = process h $ getStream h

poc :: String
poc = show $ bootstrap sampleHandler

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
