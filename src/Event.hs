{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Event (Event (), pattern Ev, event, promote, promoteSubset) where

import Data.Kind (Type)
import TypeUtils

-- An event that has a type of any of the events given in the list. Basically this is a sum type of the list of types.
data Event (as :: [Type]) where
  Ev :: Elem as a -> a -> Event as

-- Not possible
instance Show (Event '[]) where
  show = undefined

instance (Show a, Show (Event as)) => Show (Event (a : as)) where
  show (Ev Here a) = "Event[" ++ show a ++ "]"
  show (Ev (There s) a) = show (Ev s a)

event :: (Indexable as a) => a -> Event as
event = Ev position

-- Every event is also an event of a larger set of events
promote :: Event as -> Event (a : as)
promote (Ev p e) = Ev (There p) e

promoteSubset :: Subset as bs -> Event as -> Event bs
promoteSubset ss (Ev e s) = Ev (promoteElement e ss) s
