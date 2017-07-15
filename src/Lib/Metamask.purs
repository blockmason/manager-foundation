module Network.Eth.Metamask
       (
         checkStatus
       , loggedIn
       , currentUserAddress
       , METAMASK
       , MetamaskStatus(..)
       ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), maybe')
import Data.Either (Either(..), either)

foreign import data METAMASK ∷ Effect

type DummyString = String

data MetamaskStatus = LoggedOut | LoggedIn
instance showMetamaskStatus ∷ Show MetamaskStatus where
  show val = case val of
    LoggedOut → "Metamask is logged out."
    LoggedIn  → "Metamask is logged in."

foreign import checkStatusImpl ∷ ∀ e. DummyString → Eff e Boolean
foreign import currentUserImpl ∷ ∀ e. DummyString → Eff e String

checkStatus ∷ ∀ e. Eff (metamask ∷ METAMASK | e) MetamaskStatus
checkStatus = do
  res ← checkStatusImpl "has to pass a variable"
  if res then pure LoggedIn else pure LoggedOut

currentUserAddress ∷ ∀ e. Eff (metamask ∷ METAMASK | e) String
currentUserAddress = currentUserImpl "dummy"

loggedIn ∷ ∀ e. Eff (metamask ∷ METAMASK | e) Boolean
loggedIn = do
  status ← checkStatus
  case status of
    LoggedOut → pure false
    LoggedIn  → pure true
