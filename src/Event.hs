{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Event (Event (), pattern Ev, event, promote) where

import Data.Kind (Type)
import TypeUtils

data Event (as :: [Type]) where
  Ev :: Elem as a -> a -> Event as

event :: (Indexable as a) => a -> Event as
event = Ev position

promote :: Event as -> Event (a : as)
promote (Ev p e) = Ev (There p) e
