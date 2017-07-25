module Network.Eth.Foundation
       (
         FOUNDATION
       , FoundationId(..)
       , FoundationName(..)
       , FoundationError(..)
       , PendingUnification
       , printTransaction

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
       , getDepositWei
       , expirationDate
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(..))
import Data.Maybe                  (Maybe(..), maybe, isJust)
import Data.String                 as S
import Data.Array                  as A
import Data.Int                    (round, toNumber, fromString)
import Data.DateTime.Instant       (instant, toDateTime)
import Data.Time.Duration          (Milliseconds(..))
import Data.DateTime               (DateTime(..))
import Data.Array                  as A
import Network.Eth.Metamask        (loggedIn, currentUserAddress, METAMASK)
import Network.Eth                 as E

infixr 9 compose as ∘
foreign import data FOUNDATION ∷ Effect
type MonadF a = ∀ e. ExceptT FoundationError (Aff (foundation ∷ FOUNDATION, metamask ∷ METAMASK | e)) a
runMonadF = runExceptT

type PendingUnification = FoundationId

-- error
data FoundationError =
    NoMetamask
  | InvalidDebtId
  | NoFoundationId
  | TxError

instance showError ∷ Show FoundationError where
  show NoMetamask     = "NoMetamask"
  show InvalidDebtId  = "InvalidDebtId"
  show NoFoundationId = "NoFoundationId"
  show TxError        = "TxError"

newtype FoundationName = FoundationName E.StringId
instance showFoundationName ∷ Show FoundationName where
  show (FoundationName fn) = fn
fnGetName (FoundationName fn) = fn
fnMkName = FoundationName

newtype FoundationId = FoundationId { name      ∷ FoundationName
                                    , addrs ∷ Array E.EthAddress }

isValid ∷ FoundationId → Boolean
isValid = not ∘ A.null ∘ fiGetAddrs
instance showFoundationId ∷ Show FoundationId where
  show (FoundationId fi) = case isValid (FoundationId fi) of
    true  → show fi.name <> ", " <> show fi.addrs
    false → "NoValidFoundationId"

fiGetName ∷ FoundationId → FoundationName
fiGetName (FoundationId fi) = fi.name
fiGetAddrs ∷ FoundationId → Array E.EthAddress
fiGetAddrs (FoundationId fi) = fi.addrs
fiBlankId ∷ FoundationId
fiBlankId = FoundationId { name: (FoundationName ""), addrs: [] }
fiStrName = fnGetName ∘ fiGetName

type AddrLookupFn = ∀ e. (Array E.StringAddr → Eff e Unit) → E.StringId → Eff e Unit
type SingleAddrLookupFn = ∀ e. (E.StringAddr → Eff e Unit) → E.StringId → Eff e Unit
type AddrComparisonFn = ∀ e. (Boolean → Eff e Unit) → E.StringAddr → E.StringAddr → Eff e Unit
type NameLookupFn = ∀ e. (E.StringId → Eff e Unit) → E.StringAddr → Eff e Unit
type StringNumLookupFn = ∀ e. (E.StringNum → Eff e Unit) → E.StringId → Eff e Unit
type NumberLookupFn = ∀ e. (Number → Eff e Unit) → E.StringId  → Eff e Unit
type ZeroArgTx = ∀ e. (E.RawTx → Eff e Unit)                   → Eff e Unit
type OneArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String          → Eff e Unit
type TwoArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String → String → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn
foreign import areSameIdImpl ∷ AddrComparisonFn

foreign import createIdImpl ∷ OneArgTx
foreign import sentPendingImpl ∷ SingleAddrLookupFn
foreign import todoPendingImpl ∷ NameLookupFn
foreign import addPendingUnificationImpl ∷ ∀ e. E.StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import confirmPendingUnificationImpl ∷ ∀ e. E.StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import deleteAddrImpl ∷ ∀ e. E.StringAddr → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import depositWeiImpl ∷ ∀ e. E.StringId → E.StringNum → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import withdrawDepositImpl ∷ ∀ e. E.StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import getDepositWeiImpl  ∷ StringNumLookupFn
foreign import expirationDateImpl ∷ NumberLookupFn

foreign import printTransactionImpl ∷ ∀ e. Unit → Eff e Unit

checkAndInit ∷ MonadF Unit
checkAndInit = do
  li ← liftEff loggedIn
  if li
    then liftEff $ initImpl unit
    else throwError NoMetamask

currentAddr ∷ MonadF E.EthAddress
currentAddr = do
  checkAndInit
  E.EthAddress <$> liftEff currentUserAddress

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
  pure $ FoundationId { name: FoundationName name, addrs: E.EthAddress <$> addrs }

idByAddr ∷ E.EthAddress → MonadF FoundationId
idByAddr (E.EthAddress ea) = do
  checkAndInit
  name ← liftAff $ makeAff (\err succ → resolveToNameImpl succ ea)
  addrs ← liftAff $ makeAff (\err succ → resolveToAddrImpl succ name)
  pure $ FoundationId { name: FoundationName name, addrs: E.EthAddress <$> addrs }

areSameId ∷ E.EthAddress → E.EthAddress → MonadF Boolean
areSameId (E.EthAddress ea1) (E.EthAddress ea2) = do
  checkAndInit
  liftAff $ makeAff (\e s → areSameIdImpl s ea1 ea2)

createId ∷ FoundationName → MonadF E.TX
createId (FoundationName fn) = do
  checkAndInit
  (liftAff $ makeAff (\e s → createIdImpl s fn)) >>= (E.rawToTX TxError)

sentPending ∷ MonadF (Maybe E.EthAddress)
sentPending = do
  myId ← foundationId
  case myId of
    Nothing → pure Nothing
    Just mi → do
      addr ← liftAff $ makeAff (\e s → sentPendingImpl s (show $ fiGetName mi))
      if E.isNull (E.EthAddress addr) then pure Nothing else pure $ Just (E.EthAddress addr)

todoPending ∷ MonadF (Maybe FoundationName)
todoPending = do
  addr ← currentAddr
  fn ← liftAff $ makeAff (\e s → todoPendingImpl s $ E.getEa addr)
  if fn == "" then pure Nothing else pure $ Just (FoundationName fn)

addPendingUnification ∷ E.EthAddress → MonadF Unit
addPendingUnification ea = do
  checkAndInit
  liftEff $ addPendingUnificationImpl (E.getEa ea)

confirmPendingUnification ∷ FoundationName → MonadF Unit
confirmPendingUnification (FoundationName fn) = do
  checkAndInit
  liftEff $ confirmPendingUnificationImpl fn

deleteAddr ∷ E.EthAddress → MonadF Unit
deleteAddr (E.EthAddress ea) = do
  checkAndInit
  liftEff $ deleteAddrImpl ea

depositWei ∷ E.Wei → MonadF Unit
depositWei w = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → liftEff $ depositWeiImpl ((fnGetName ∘ fiGetName) fi) (E.weiStr w)

withdrawDeposit ∷ MonadF Unit
withdrawDeposit = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → liftEff $ withdrawDepositImpl ((fnGetName ∘fiGetName) fi)

getDepositWei ∷ MonadF (Maybe E.Wei)
getDepositWei = do
  mId ← foundationId
  case mId of
    Nothing → pure Nothing
    Just i  → (Just ∘ E.mkWei) <$>
      (liftAff $ makeAff (\e s → getDepositWeiImpl s (fiStrName i)))

expirationDate ∷ MonadF (Maybe DateTime)
expirationDate = do
  mId ← foundationId
  case mId of
    Nothing → pure Nothing
    Just i  → do
      timestamp ← (liftAff $ makeAff (\e s → expirationDateImpl s (fiStrName i)))
      pure $ secsToDT timestamp
        where secsToDT secs = toDateTime <$> (instant (Milliseconds $ secs * 1000.0))

printTransaction ∷ MonadF Unit
printTransaction = do
  liftEff $ printTransactionImpl unit
