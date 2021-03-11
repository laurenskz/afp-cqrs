{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE GADTs #-} 

module Lib
    ( someFunc
    ) where

import Control.Monad.State.Lazy
import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- class Event where
--     CommitTime = 

-- data Player = Tic | Tac

-- data TickBoxEvent = {
--     Player: Player
--     Square: (Int, Int)
-- }
-- deriving (Event)

class Command c where


data BankAccountCommandHandler
type BankAccountId = Int
type Amount = Int
type Balance = Int
data NewTransactionCommand = NewTransactionCommand BankAccountId Amount
instance Command NewTransactionCommand -- TODO deriving
data NewSubscriptionCommand = NewSubscriptionCommand BankAccountId Amount
instance Command NewSubscriptionCommand -- TODO deriving
data BankAccountUpdatedEvent = BankAccountUpdatedEvent BankAccountId Balance

-- class CommandHandler s where
--     handle :: (Command c) => c -> State s ()

-- instance CommandHandler Int where
--   handle (NewTransactionCommand id amount) = return ()

-- type HandlerFunc = (Command c) => c -> State Int ()

data Handlers s t hs where 
    Handler :: (t -> State s ()) -> Handlers s t' hs' -> Handlers s t (Handlers s t' hs')
    EndHandlers :: Handlers s () ()
data CommandHandler s hs = CommandHandler s hs

bankAccountHandler :: CommandHandler Integer (Handlers Balance NewTransactionCommand (Handlers Balance NewSubscriptionCommand (Handlers Balance NewSubscriptionCommand (Handlers Balance () ()))))
bankAccountHandler = CommandHandler 0 
    $ Handler (\(NewTransactionCommand id amount) -> modify (\s -> s - amount))
    $ Handler (\(NewSubscriptionCommand id amount) -> modify (\s -> s - amount))
    $ Handler (\(NewSubscriptionCommand id amount) -> return ())
    EndHandlers

-- instance CommandHandler Int where
--    handle (NewTransactionCommand id amount) = do
--        modify (\s -> s - amount)
--        s <- get
--        raise $ BankAccountUpdatedEvent id $ s
--        return ()
--    handle (NewSubscriptionCommand id amount) = do
--        modify (\s -> s - amount)
--        s <- get
--        raise $ BankAccountUpdatedEvent id $ s
--        return ()

-- instance Projector BankAccountProjector (BackAccountUpdatedEvent, Int) where
--     handle (BackAccountUpdatedEvent id saldo) = do
--         modifyId id (\_ -> saldo)
--     hnalde (BankAccountBlockedEvent id) do
--         delete id
-- -- [
-- --     {id: state(int)}
-- -- ]
