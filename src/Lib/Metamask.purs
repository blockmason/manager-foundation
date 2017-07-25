module Network.Eth.Metamask
       (
         checkStatus
       , loggedIn
       , currentUserAddress
       , printTransaction
       , METAMASK
       , MetamaskStatus(..)
       ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), maybe')
import Data.Either (Either(..), either)

foreign import data METAMASK ∷ Effect

data MetamaskStatus = LoggedOut | LoggedIn
instance showMetamaskStatus ∷ Show MetamaskStatus where
  show val = case val of
    LoggedOut → "Metamask is logged out."
    LoggedIn  → "Metamask is logged in."

foreign import checkStatusImpl ∷ ∀ e. Unit → Eff e Boolean
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e String
foreign import printTransactionImpl ∷ ∀ e. Unit → Eff e Unit

checkStatus ∷ ∀ e. Eff (metamask ∷ METAMASK | e) MetamaskStatus
checkStatus = do
  res ← checkStatusImpl unit
  if res then pure LoggedIn else pure LoggedOut

currentUserAddress ∷ ∀ e. Eff (metamask ∷ METAMASK | e) String
currentUserAddress = currentUserImpl unit

loggedIn ∷ ∀ e. Eff (metamask ∷ METAMASK | e) Boolean
loggedIn = do
  status ← checkStatus
  case status of
    LoggedOut → pure false
    LoggedIn  → pure true

printTransaction ∷ ∀ e. Eff (metamask ∷ METAMASK | e) Unit
printTransaction = do
  printTransactionImpl unit
