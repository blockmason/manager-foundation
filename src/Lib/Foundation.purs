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
       , eaMkAddr
       , fiBlankId
       , fiGetName
       , fiGetAddrs
       , fnGetName
       , fnMkName

       , currentAddr
       , foundationId

       , runMonadF
       , idByAddr
       , idByName
       , areSameId

       , createId
       , sentPending
       , todoPending
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
import Data.Maybe                  (Maybe(..))
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
eaMkAddr = EthAddress
isNull ∷ EthAddress → Boolean
isNull (EthAddress ea) = ea == "0x0"

newtype FoundationName = FoundationName StringId
instance showFoundationName ∷ Show FoundationName where
  show (FoundationName fn) = fn
fnGetName (FoundationName fn) = fn
fnMkName = FoundationName

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
type SingleAddrLookupFn = ∀ e. (StringAddr → Eff e Unit) → StringId → Eff e Unit
type AddrComparisonFn = ∀ e. (Boolean → Eff e Unit) → StringAddr → StringAddr → Eff e Unit
type NameLookupFn = ∀ e. (StringId → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn
foreign import areSameIdImpl ∷ AddrComparisonFn

foreign import createIdImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import sentPendingImpl ∷ SingleAddrLookupFn
foreign import todoPendingImpl ∷ NameLookupFn
foreign import addPendingUnificationImpl ∷ ∀ e. StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
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

foundationId ∷ MonadF (Maybe FoundationId)
foundationId = do
  addr ← currentAddr
  myId ← idByAddr addr
  if A.null (fiGetAddrs myId)
    then pure $ Nothing
    else pure $ Just myId

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

sentPending ∷ MonadF (Maybe EthAddress)
sentPending = do
  myId ← foundationId
  case myId of
    Nothing → pure Nothing
    Just mi → do
      addr ← liftAff $ makeAff (\e s → sentPendingImpl s (show $ fiGetName mi))
      if isNull (EthAddress addr) then pure Nothing else pure $ Just (EthAddress addr)

todoPending ∷ MonadF (Maybe FoundationName)
todoPending = do
  addr ← currentAddr
  fn ← liftAff $ makeAff (\e s → todoPendingImpl s $ getEa addr)
  if fn == "" then pure Nothing else pure $ Just (FoundationName fn)

addPendingUnification ∷ EthAddress → MonadF Unit
addPendingUnification ea = do
  checkAndInit
  liftEff $ addPendingUnificationImpl (getEa ea)

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
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → liftEff $ depositWeiImpl ((fnGetName <<< fiGetName) fi) (weiGet w)

withdrawDeposit ∷ MonadF Unit
withdrawDeposit = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → liftEff $ withdrawDepositImpl ((fnGetName <<<fiGetName) fi)
