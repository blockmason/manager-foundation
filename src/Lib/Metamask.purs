module Network.Eth.Metamask
       (
         checkStatus
       , loggedIn
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

loggedIn ∷ MetamaskStatus → Boolean
loggedIn mms = case mms of
  LoggedOut → false
  LoggedIn  → true

foreign import checkStatusImpl ∷ ∀ e. DummyString → Eff e Boolean

checkStatus ∷ ∀ e. Eff e MetamaskStatus
checkStatus = do
  res ← checkStatusImpl "has to pass a variable"
  if res then pure LoggedIn else pure LoggedOut
