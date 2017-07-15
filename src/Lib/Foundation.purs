module Network.Eth.Foundation
       (
         FoundationId(..)
       , EthAddress(..)
       , Error(..)
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(..))
import Data.String                 (localeCompare)
import Network.Eth.Metamask        (loggedIn, currentUserAddress, METAMASK)

infixr 9 compose as ∘
foreign import data FOUNDATION ∷ Effect

type DummyVal = String
type MonadF a = ∀ e. ExceptT Error (Aff (foundation ∷ FOUNDATION, metamask ∷ METAMASK | e)) a

data Error = NoMetamask
instance showError ∷ Show Error where
  show NoMetamask = "NoMetamask: Metamask not logged in."

newtype EthAddress = EthAddress String
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = localeCompare ua1 ua2
getUa ∷ EthAddress → String
getUa (EthAddress ua) = ua

newtype FoundationId = FoundationId { name      ∷ String
                                    , addresses ∷ Array EthAddress }

foreign import initImpl ∷ ∀ e. DummyVal → Eff (foundation ∷ FOUNDATION | e) Unit
--foreign import resolveToAddrImpl
--foreign import resolveToNameImpl

checkAndInit ∷ MonadF Unit
checkAndInit = do
  li ← liftEff loggedIn
  if li then liftEff $ initImpl "" else throwError NoMetamask

currentUser ∷ MonadF EthAddress
currentUser = do
  checkAndInit
  EthAddress <$> liftEff currentUserAddress
