module Network.Eth.Foundation
       (
         FOUNDATION
       , FoundationId(..)
       , FoundationName(..)
       , EthAddress(..)
       , Error(..)
       , Wei(..)
       , PendingUnification
       , StringAddr
       , StringId
       , fiBlankId
       , fiGetAddrs

       , currentAddr
       , foundationId

       , runMonadF
       , idByAddr
       , idByName
       , areSameId

       , createId
       , addPendingUnification
       , confirmPendingUnification
       , deleteAddr

       , depositWei
       , withdrawDeposit
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

type StringAddr = String
type StringId = String
type PendingUnification = FoundationId

-- error
data Error =
    NoMetamask
  | InvalidDebtId
  | NoFoundationId

instance showError ∷ Show Error where
  show NoMetamask     = "NoMetamask"
  show InvalidDebtId  = "InvalidDebtId"
  show NoFoundationId = "NoFoundationId"

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

isValid ∷ FoundationId → Boolean
isValid = not <<< A.null <<< fiGetAddrs
instance showFoundationId ∷ Show FoundationId where
  show (FoundationId fi) = case isValid (FoundationId fi) of
    true  → show fi.name <> ", " <> show fi.addrs
    false → "NoValidFoundationId"

fiGetName ∷ FoundationId → FoundationName
fiGetName (FoundationId fi) = fi.name
fiGetAddrs ∷ FoundationId → Array EthAddress
fiGetAddrs (FoundationId fi) = fi.addrs
fiBlankId ∷ FoundationId
fiBlankId = FoundationId { name: (FoundationName ""), addrs: [] }

newtype Wei = Wei Number
instance showWei ∷ Show Wei where
  show (Wei num) = show num
weiGet ∷ Wei → Number
weiGet (Wei num) = num

type AddrLookupFn = ∀ e. (Array StringAddr → Eff e Unit) → StringId → Eff e Unit
type AddrComparisonFn = ∀ e. (Boolean → Eff e Unit) → StringAddr → StringAddr → Eff e Unit
type NameLookupFn = ∀ e. (StringId → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn
foreign import areSameIdImpl ∷ AddrComparisonFn

foreign import createIdImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import addPendingUnificationImpl ∷ ∀ e. StringId → StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import confirmPendingUnificationImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import deleteAddrImpl ∷ ∀ e. StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import depositWeiImpl ∷ ∀ e. StringId → Number → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import withdrawDepositImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit

checkAndInit ∷ MonadF Unit
checkAndInit = do
  li ← liftEff loggedIn
  if li
    then liftEff $ initImpl unit
    else throwError NoMetamask

currentAddr ∷ MonadF EthAddress
currentAddr = do
  checkAndInit
  EthAddress <$> liftEff currentUserAddress

foundationId ∷ MonadF FoundationId
foundationId = do
  addr ← currentAddr
  myId ← idByAddr addr
  if A.null (fiGetAddrs myId) then throwError NoFoundationId else pure myId

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

areSameId ∷ EthAddress → EthAddress → MonadF Boolean
areSameId (EthAddress ea1) (EthAddress ea2) = do
  checkAndInit
  liftAff $ makeAff (\e s → areSameIdImpl s ea1 ea2)

createId ∷ FoundationName → MonadF Unit
createId (FoundationName fn) = do
  checkAndInit
  liftEff $ createIdImpl fn

addPendingUnification ∷ FoundationName → EthAddress → MonadF Unit
addPendingUnification (FoundationName fn) (EthAddress ea) = do
  checkAndInit
  liftEff $ addPendingUnificationImpl fn ea

confirmPendingUnification ∷ FoundationName → MonadF Unit
confirmPendingUnification (FoundationName fn) = do
  checkAndInit
  liftEff $ confirmPendingUnificationImpl fn

deleteAddr ∷ EthAddress → MonadF Unit
deleteAddr (EthAddress ea) = do
  checkAndInit
  liftEff $ deleteAddrImpl ea


depositWei ∷ Wei → MonadF Unit
depositWei w = do
  (FoundationName name) ← (fiGetName <$> foundationId)
  liftEff $ depositWeiImpl name (weiGet w)

withdrawDeposit ∷ MonadF Unit
withdrawDeposit = do
  (FoundationName name) ← (fiGetName <$> foundationId)
  liftEff $ withdrawDepositImpl name
