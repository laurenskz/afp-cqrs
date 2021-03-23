{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Event (Event (), pattern Ev, event, promote) where

import Data.Kind (Type)
import TypeUtils

data Event (as :: [Type]) where
  Ev :: Elem as a -> a -> Event as

instance Show (Event '[]) where
  show = undefined

instance (Show a, Show (Event as)) => Show (Event (a : as)) where
  show (Ev Here a) = "Event[" ++ show a ++ "]"
  show (Ev (There s) a) = show (Ev s a)

event :: (Indexable as a) => a -> Event as
event = Ev position

promote :: Event as -> Event (a : as)
promote (Ev p e) = Ev (There p) e
