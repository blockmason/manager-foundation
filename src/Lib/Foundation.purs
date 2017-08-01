module Network.Eth.Foundation
       (
         FOUNDATION
       , FoundationId(..)
       , FoundationName(..)
       , FoundationError(..)
       , PendingUnification

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
       , getWeiToExtend
       , extendIdOneYear
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
  | NameInUse
  | AddrInUse

instance showError ∷ Show FoundationError where
  show NoMetamask     = "NoMetamask"
  show InvalidDebtId  = "InvalidDebtId"
  show NoFoundationId = "NoFoundationId"
  show TxError        = "TxError"
  show NameInUse      = "NameInUse"
  show AddrInUse      = "AddrInUse"

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
type ZeroArgLookupFn = ∀ e. (String → Eff e Unit) → Eff e Unit
type ZeroArgTx = ∀ e. (E.RawTx → Eff e Unit)                   → Eff e Unit
type OneArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String          → Eff e Unit
type TwoArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String → String → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn
foreign import areSameIdImpl ∷ AddrComparisonFn

foreign import createIdImpl        ∷ OneArgTx
foreign import sentPendingImpl     ∷ SingleAddrLookupFn
foreign import todoPendingImpl     ∷ NameLookupFn
foreign import addPendingUnificationImpl ∷ OneArgTx
foreign import confirmPendingUnificationImpl ∷ OneArgTx
foreign import deleteAddrImpl      ∷ OneArgTx
foreign import depositWeiImpl      ∷ TwoArgTx
foreign import withdrawDepositImpl ∷ OneArgTx
foreign import getDepositWeiImpl   ∷ StringNumLookupFn
foreign import getWeiToExtendImpl  ∷ ZeroArgLookupFn
foreign import extendIdOneYearImpl ∷ TwoArgTx
foreign import expirationDateImpl  ∷ NumberLookupFn

checkMM ∷ MonadF Unit
checkMM = do
  li ← liftEff loggedIn
  if li
    then liftEff $ initImpl unit
    else throwError NoMetamask

currentAddr ∷ MonadF E.EthAddress
currentAddr = do
  checkMM
  liftEff currentUserAddress

foundationId ∷ MonadF (Maybe FoundationId)
foundationId = do
  addr ← currentAddr
  myId ← idByAddr addr
  if A.null (fiGetAddrs myId)
    then pure $ Nothing
    else pure $ Just myId

idByName ∷ FoundationName → MonadF FoundationId
idByName (FoundationName name) = do
  checkMM
  addrs ← liftAff $ makeAff (\err succ → resolveToAddrImpl succ name)
  pure $ FoundationId { name: FoundationName name, addrs: E.EthAddress <$> addrs }

idByAddr ∷ E.EthAddress → MonadF FoundationId
idByAddr (E.EthAddress ea) = do
  checkMM
  name ← liftAff $ makeAff (\err succ → resolveToNameImpl succ ea)
  addrs ← liftAff $ makeAff (\err succ → resolveToAddrImpl succ name)
  pure $ FoundationId { name: FoundationName name, addrs: E.EthAddress <$> addrs }

areSameId ∷ E.EthAddress → E.EthAddress → MonadF Boolean
areSameId (E.EthAddress ea1) (E.EthAddress ea2) = do
  checkMM
  liftAff $ makeAff (\e s → areSameIdImpl s ea1 ea2)

--throws AddrInUse if Address is in use
isFreshAddr ∷ E.EthAddress → MonadF Unit
isFreshAddr ea = do
  id' ← idByAddr ea
  if isValid id' then throwError AddrInUse else pure unit

--make sure name and current address haven't been used prior (ie aren't valid)
--throws NameInUse and AddrInUse
isFreshNameAndUser ∷ FoundationName → MonadF Unit
isFreshNameAndUser fn = do
  currentAddr >>= isFreshAddr
  id' ← idByName fn
  if isValid id' then throwError NameInUse else pure unit

createId ∷ FoundationName → MonadF E.TX
createId fn = do
  isFreshNameAndUser fn
  (liftAff $ makeAff (\e s → createIdImpl s (fnGetName fn))) >>= (E.rawToTX TxError)

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

addPendingUnification ∷ E.EthAddress → MonadF E.TX
addPendingUnification ea = do
  checkMM
  isFreshAddr ea
  tx ← liftAff $ makeAff (\_ s → addPendingUnificationImpl s $ E.getEa ea)
  E.rawToTX TxError tx

confirmPendingUnification ∷ FoundationName → MonadF E.TX
confirmPendingUnification (FoundationName fn) = do
  checkMM
  tx ← liftAff $ makeAff (\_ s → confirmPendingUnificationImpl s fn)
  E.rawToTX TxError tx

deleteAddr ∷ E.EthAddress → MonadF E.TX
deleteAddr (E.EthAddress ea) = do
  checkMM
  tx ← liftAff $ makeAff (\_ s → deleteAddrImpl s ea)
  E.rawToTX TxError tx

depositWei ∷ E.Wei → MonadF E.TX
depositWei w = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → (liftAff $ makeAff (\_ s → depositWeiImpl s ((fnGetName ∘ fiGetName) fi) (E.weiStr w))) >>= (E.rawToTX TxError)

withdrawDeposit ∷ MonadF E.TX
withdrawDeposit = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → do
      tx ← liftAff $ makeAff (\_ s → withdrawDepositImpl s (fiStrName fi))
      E.rawToTX TxError tx

getDepositWei ∷ MonadF (Maybe E.Wei)
getDepositWei = do
  mId ← foundationId
  case mId of
    Nothing → pure Nothing
    Just i  → (Just ∘ E.mkWei) <$>
      (liftAff $ makeAff (\_ s → getDepositWeiImpl s (fiStrName i)))

getWeiToExtend ∷ MonadF E.Wei
getWeiToExtend =
  E.mkWei <$> (liftAff $ makeAff (\_ s → getWeiToExtendImpl s))

extendIdOneYear ∷ MonadF E.TX
extendIdOneYear = do
  mId  ← foundationId
  wStr ← E.weiStr <$> getWeiToExtend
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → do
      tx ← liftAff $ makeAff (\_ s → extendIdOneYearImpl s (fiStrName fi) wStr)
      E.rawToTX TxError tx

expirationDate ∷ MonadF (Maybe DateTime)
expirationDate = do
  mId ← foundationId
  case mId of
    Nothing → pure Nothing
    Just i  → do
      timestamp ← (liftAff $ makeAff (\_ s → expirationDateImpl s (fiStrName i)))
      pure $ secsToDT timestamp
        where secsToDT secs = toDateTime <$> (instant (Milliseconds $ secs * 1000.0))
