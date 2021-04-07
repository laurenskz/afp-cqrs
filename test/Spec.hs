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
newtype OrderSucceededEvent = OrderSucceededEvent (ProductId, BankAccountId, Amount) deriving (Show)
newtype OrderFailedEvent    = OrderFailedEvent    (ProductId, BankAccountId, Amount) deriving (Show)
newtype ProductCreatedEvent = ProductCreatedEvent (ProductId, Name, Price) deriving (Show)
newtype StockAddedEvent     = StockAddedEvent     (ProductId, Amount) deriving (Show)
newtype BalanceAddedEvent   = BalanceAddedEvent   (BankAccountId, Balance) deriving (Show)

-- TODO make lookups state native
newtype BankAccounts = BankAccounts (Map.Map BankAccountId Balance) deriving (Show)
newtype Products     = Products     (Map.Map ProductId (Name, Price, Amount)) deriving (Show)

type POCEvents = '[OrderPlacedEvent, OrderSucceededEvent, OrderFailedEvent, ProductCreatedEvent, StockAddedEvent, BalanceAddedEvent]
type POCStates = '[BankAccounts, Products]

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

end :: HandlersT '[] POCEvents POCStates
end = HNil

handlers = addHandler balanceAddedHandler
         $ addHandler stockAddedHandler
         $ addHandler productCreatedHandler
         $ addHandler orderPlacedHandler
       end

initialStates = TCons (Identity $ BankAccounts empty) (TCons (Identity $ Products empty) TNil)

application = Application handlers (emptyTypeList []) (emptyTypeList [])

pocEvents :: [Event POCEvents]
pocEvents = [ event $ ProductCreatedEvent (1, "Fiets", 100)
            , event $ StockAddedEvent (1, 1)
            , event $ BalanceAddedEvent (1, 100)
            , event $ OrderPlacedEvent (1, 1, 1)
            ]

runPoc :: IO ()
runPoc = do
  finalStates <- executeCompletely application pocEvents initialStates
  print finalStates

main :: IO ()
main = runPoc
