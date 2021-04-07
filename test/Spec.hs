{-# LANGUAGE DataKinds #-}

import TypeUtils
import Event
import EventHandler
import Application
import Execution
import Control.Monad.Identity
import Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

type ProductId     = Int
type BankAccountId = Int
type Amount        = Int
type Balance       = Int
type Price         = Int

type Name = String
type Order = (ProductId, BankAccountId, Amount)

newtype OrderPlacedEvent    = OrderPlacedEvent    Order deriving (Show)
newtype OrderSucceededEvent = OrderSucceededEvent Order deriving (Show)
newtype OrderFailedEvent    = OrderFailedEvent    Order deriving (Show)
newtype ProductCreatedEvent = ProductCreatedEvent (ProductId, Name, Price) deriving (Show)
newtype StockAddedEvent     = StockAddedEvent     (ProductId, Amount) deriving (Show)
newtype BalanceAddedEvent   = BalanceAddedEvent   (BankAccountId, Balance) deriving (Show)

-- TODO make lookups state native
newtype BankAccounts = BankAccounts (Map.Map BankAccountId Balance) deriving (Show)
newtype Products     = Products     (Map.Map ProductId (Name, Price, Amount)) deriving (Show)
newtype Orders       = Orders       [(Order, Bool)] deriving (Show)

type POCEvents = '[OrderPlacedEvent, OrderSucceededEvent, OrderFailedEvent, ProductCreatedEvent, StockAddedEvent, BalanceAddedEvent]
type POCStates = '[BankAccounts, Products, Orders]

balanceAddedHandler :: EventHandler '[BankAccounts] '[BankAccounts] BalanceAddedEvent '[] ()
balanceAddedHandler = do
  (BalanceAddedEvent (id, amount)) <- handle
  -- TODO modify
  (BankAccounts state) <- readState
  writeState $ BankAccounts $ Map.insertWith (+) id amount state

stockAddedHandler :: EventHandler '[Products] '[Products] StockAddedEvent '[] ()
stockAddedHandler = do
  (StockAddedEvent (id, amount)) <- handle
  (Products state) <- readState
  writeState $ Products $ Map.insertWith (\(_, _, a) (n, p, a') -> (n, p, a + a')) id ("", 0, amount) state

productCreatedHandler :: EventHandler '[Products] '[Products] ProductCreatedEvent '[] ()
productCreatedHandler = do
  (ProductCreatedEvent (id, name, price)) <- handle
  (Products state) <- readState
  writeState $ Products $ Map.insertWith (\(_, _, a) (n, p, _) -> (n, p, a)) id (name, price, 0) state

orderPlacedHandler :: EventHandler '[BankAccounts, Products] '[BankAccounts, Products] OrderPlacedEvent '[OrderSucceededEvent, OrderFailedEvent] ()
orderPlacedHandler = do
  (OrderPlacedEvent o@(productId, bankAccountId, amount)) <- handle
  (BankAccounts accounts) <- readState
  (Products products)     <- readState

  let success = do
       (n, price, stock) <- Map.lookup productId products
       balance           <- Map.lookup bankAccountId accounts
       return $ stock >= amount && balance >= amount * price

  case success of
    Just True -> do
      let (n, price, stock) = products Map.! productId
      writeState $ Products $ Map.insert productId (n, price, stock - amount) products
      writeState $ BankAccounts $ Map.adjust (\s -> s - amount * price) bankAccountId accounts
      raiseEvent $ OrderSucceededEvent o
    _ -> raiseEvent $ OrderFailedEvent o

orderFailedHandler :: EventHandler '[Orders] '[Orders] OrderFailedEvent '[] ()
orderFailedHandler = do
  (OrderFailedEvent o) <- handle
  (Orders os) <- readState
  writeState $ Orders $ (o, False) : os

orderSucceededHandler :: EventHandler '[Orders] '[Orders] OrderSucceededEvent '[] ()
orderSucceededHandler = do
  (OrderSucceededEvent o) <- handle
  (Orders os) <- readState
  writeState $ Orders $ (o, True) : os

end :: HandlersT '[] POCEvents POCStates
end = HNil

handlers = addHandler balanceAddedHandler
         $ addHandler stockAddedHandler
         $ addHandler productCreatedHandler
         $ addHandler orderPlacedHandler
         $ addHandler orderFailedHandler
         $ addHandler orderSucceededHandler
       end

initialStates = TCons (Identity $ BankAccounts empty) 
              $ TCons (Identity $ Products empty) 
              $ TCons (Identity $ Orders []) 
              TNil

application = Application handlers (emptyTypeList []) (emptyTypeList [])

pocEvents1 :: [Event POCEvents]
pocEvents1 = [ event $ ProductCreatedEvent (1, "Fiets", 100)
             , event $ StockAddedEvent (1, 1)
             , event $ BalanceAddedEvent (1, 100)
             , event $ OrderPlacedEvent (1, 1, 1)
             ]

pocEvents2 :: [Event POCEvents]
pocEvents2 = [ event $ ProductCreatedEvent (1, "Fiets", 110)
             , event $ StockAddedEvent (1, 1)
             , event $ BalanceAddedEvent (1, 100)
             , event $ OrderPlacedEvent (1, 1, 1)
             ]

pocEvents3 :: [Event POCEvents]
pocEvents3 = [ event $ ProductCreatedEvent (1, "Fiets", 100)
             , event $ StockAddedEvent (1, 1)
             , event $ BalanceAddedEvent (1, 100)
             , event $ OrderPlacedEvent (1, 1, 2)
             ]

runPoc evs = do
  finalStates <- executeCompletely application evs initialStates
  print finalStates


tests :: TestTree
tests = testGroup "Implementations" 
  [ testCase "Test 1" $ runPoc pocEvents1
  , testCase "Test 2" $ runPoc pocEvents2
  , testCase "Test 3" $ runPoc pocEvents3
  ]

main = defaultMain tests
