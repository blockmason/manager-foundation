module Network.Eth.Foundation
       (
         FOUNDATION
       , FoundationId(..)
       , FoundationName(..)
       , EthAddress(..)
       , Error(..)
       , StringAddr
       , StringId
       , runMonadF
       , idByAddr
       , idByName
       , currentAddr
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(..))
import Data.String                 (localeCompare)
import Data.Array                  as A
import Network.Eth.Metamask        (loggedIn, currentUserAddress, METAMASK)

infixr 9 compose as ∘
foreign import data FOUNDATION ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (foundation ∷ FOUNDATION, metamask ∷ METAMASK | e)) a
runMonadF = runExceptT

type DummyVal = String
type StringAddr = String
type StringId = String

-- error
data Error =
    NoMetamask
  | InvalidDebtId
  | NoFoundationId

instance showError ∷ Show Error where
  show NoMetamask     = "FriendInDebtError: Metamask not logged in."
  show InvalidDebtId  = "FriendInDebtError: InvalidDebtId"
  show NoFoundationId = "FriendInDebtError: NoFoundationId"

newtype EthAddress = EthAddress StringAddr
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = localeCompare ua1 ua2
getEa ∷ EthAddress → String
getEa (EthAddress ea) = ea

newtype FoundationName = FoundationName StringId
instance showFoundationName ∷ Show FoundationName where
  show (FoundationName fn) = fn

newtype FoundationId = FoundationId { name      ∷ FoundationName
                                    , addrs ∷ Array EthAddress }
instance showFoundationId ∷ Show FoundationId where
  show (FoundationId fi) = show fi.name <> ", " <> show fi.addrs
fiGetId ∷ FoundationId → FoundationName
fiGetId (FoundationId fi) = fi.name

type AddrLookupFn = ∀ e. (Array StringAddr → Eff e Unit) → StringId → Eff e Unit
type NameLookupFn = ∀ e. (StringId → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. DummyVal → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn

foreign import createIdImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import addPendingUnificationImpl ∷ ∀ e. StringId → StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
--confirmPendingUnificationImpl Eff
--areSameId addr1 addr2 Aff
--deleteAddr addr Eff


checkAndInit ∷ MonadF Unit
checkAndInit = do
  li ← liftEff loggedIn
  if li
    then liftEff $ initImpl ""
    else throwError NoMetamask

currentAddr ∷ MonadF EthAddress
currentAddr = do
  checkAndInit
  EthAddress <$> liftEff currentUserAddress

idByName ∷ FoundationName → MonadF FoundationId
idByName (FoundationName name) = do
  checkAndInit
  addrs ← liftAff $ makeAff (\err succ → resolveToAddrImpl succ name)
  pure $ FoundationId { name: FoundationName name, addrs: EthAddress <$> addrs }

idByAddr ∷ EthAddress → MonadF FoundationId
idByAddr (EthAddress ea) = do
  checkAndInit
  name ← liftAff $ makeAff (\err succ → resolveToNameImpl succ ea)
  addrs ← liftAff $ makeAff (\err succ → resolveToAddrImpl succ name)
  pure $ FoundationId { name: FoundationName name, addrs: EthAddress <$> addrs }

createId ∷ FoundationName → MonadF Unit
createId (FoundationName fn) = do
  checkAndInit
  liftEff $ createIdImpl fn

addPendingUnification ∷ FoundationName → EthAddress → MonadF Unit
addPendingUnification (FoundationName fn) (EthAddress ea) = do
  checkAndInit
  liftEff $ addPendingUnificationImpl fn ea
