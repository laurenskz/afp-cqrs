{-# LANGUAGE DataKinds #-}

import TypeUtils
import Event
import EventHandler
import Application
import Execution
import Control.Monad.Identity
import Data.Map as Map

type ProductId     = Int
type BankAccountId = Int
type Amount        = Int
type Balance       = Int
type Price         = Int

type Name = String

newtype OrderPlacedEvent    = OrderPlacedEvent    (ProductId, BankAccountId, Amount) deriving (Show)
newtype ProductCreatedEvent = ProductCreatedEvent (ProductId, Name, Price) deriving (Show) 
newtype StockAddedEvent     = StockAddedEvent     (ProductId, Amount) deriving (Show)
newtype BalanceAddedEvent   = BalanceAddedEvent   (BankAccountId, Balance) deriving (Show)

-- TODO make lookups state native
newtype BankAccounts = BankAccounts (Map.Map BankAccountId Balance) deriving (Show)
newtype Products     = Products     (Map.Map ProductId (Name, Price, Amount))

type POCEvents = '[OrderPlacedEvent, ProductCreatedEvent, StockAddedEvent, BalanceAddedEvent]
type POCStates = '[BankAccounts, Products]

balanceAddedHandler :: EventHandler '[BankAccounts] '[] BalanceAddedEvent '[] ()
balanceAddedHandler = do
  (BalanceAddedEvent (id, amount)) <- handle
  -- TODO modify
  (BankAccounts state) <- readState
  writeState $ BankAccounts $ Map.insertWith (+) id amount state

stockAddedHandler :: EventHandler '[Products] '[] StockAddedEvent '[] ()
stockAddedHandler = do
  (StockAddedEvent (id, amount)) <- handle
  (Products state) <- readState
  writeState $ Products $ Map.insertWith (\(_, _, a) (n, p, a') -> (n, p, a + a')) id ("", 0, amount) state

productCreatedHandler :: EventHandler '[Products] '[] ProductCreatedEvent '[] ()
productCreatedHandler = do
  (ProductCreatedEvent (id, name, price)) <- handle
  (Products state) <- readState
  writeState $ Products $ Map.insertWith (\(_, _, a) (n, p, _) -> (n, p, a)) id (name, price, 0) state

orderPlacedHandler :: EventHandler '[BankAccounts, Products] '[] OrderPlacedEvent '[] ()
orderPlacedHandler = do
  (OrderPlacedEvent (productId, bankAccountId, amount)) <- handle
  (BankAccounts accounts) <- readState
  (Products products) <- readState

  let product = Map.lookup productId products
  let account = Map.lookup bankAccountId accounts

  return ()

  -- check stock
  -- check balance

  -- mutate states and raise success or failed event

-- pocHandler1 :: EventHandler '[Balance] '[Price, Balance] ReceivedMoneyEvent '[WonSomethingEvent] ()
-- pocHandler1 = do
--   (ReceivedMoneyEvent x) <- handle
--   (Balance current) <- readState
--   writeState (Balance (current + x))
--   raiseEvent (WonSomethingEvent ("Price! being added to balance of " ++ show current))

-- pocHandler2 :: EventHandler '[Price] '[User] WonSomethingEvent '[] ()
-- pocHandler2 = do
--   (WonSomethingEvent name) <- handle
--   writeState (User ("User with a nice price!:" ++ name))

-- type POCHandlers =
--   '[ 'KHandler '[Balance] '[Price, Balance] ReceivedMoneyEvent '[WonSomethingEvent],
--      'KHandler '[Price] '[User] WonSomethingEvent '[]
--    ]

-- handlers :: HandlersT POCHandlers POCEvents POCStates
-- handlers = addHandler pocHandler1 (addHandler pocHandler2 HNil)

-- initialStates :: TypedList POCStates Identity
-- initialStates = TCons (Identity $ Balance 0) (TCons (Identity $ Price "") (TCons (Identity $ User "") TNil))

-- application :: Application POCHandlers POCEvents POCStates
-- application = Application handlers (emptyTypeList []) (emptyTypeList [])

-- pocEvents :: [Event POCEvents]
-- pocEvents = [event $ ReceivedMoneyEvent 3]

-- runPoc :: IO ()
-- runPoc = do
--   finalStates <- executeCompletely application pocEvents initialStates
--   print finalStates

main :: IO ()
main = return ()
