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
       , mkWei
       , weiStr
       , weiShowEth
       , zeroWei

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

infixr 9 compose as ∘
foreign import data FOUNDATION ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (foundation ∷ FOUNDATION, metamask ∷ METAMASK | e)) a
runMonadF = runExceptT

type StringAddr = String
type StringId = String
type StringNum = String
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
  compare (EthAddress ua1) (EthAddress ua2) = S.localeCompare ua1 ua2
getEa ∷ EthAddress → String
getEa (EthAddress ea) = ea
eaMkAddr = EthAddress
isNull ∷ EthAddress → Boolean
isNull (EthAddress ea) = ea == "0x0000000000000000000000000000000000000000"

newtype FoundationName = FoundationName StringId
instance showFoundationName ∷ Show FoundationName where
  show (FoundationName fn) = fn
fnGetName (FoundationName fn) = fn
fnMkName = FoundationName

newtype FoundationId = FoundationId { name      ∷ FoundationName
                                    , addrs ∷ Array EthAddress }

isValid ∷ FoundationId → Boolean
isValid = not ∘ A.null ∘ fiGetAddrs
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
fiStrName = fnGetName ∘ fiGetName

newtype Wei = Wei String
mkWei ∷ String → Wei
mkWei ""  = Wei "0"
mkWei str = if isInt str then Wei str else zeroWei
  where isInt "" = true
        isInt s  = (isJust (fromString $ S.take 9 s)) && (isInt $ S.drop 9 s)
zeroWei ∷ Wei
zeroWei = Wei "0"
instance showWei ∷ Show Wei where
  show (Wei s) = s
weiGet ∷ Wei → String
weiGet (Wei s) = s
weiStr ∷ Wei → String
weiStr (Wei s) = s
weiShowEth ∷ Wei → String
weiShowEth (Wei w) =
  let len = S.length w
  in if len > 18
     then dropZeros $ S.take (len-18) w <> "." <> S.drop (len-18) w
     else dropZeros $ "0." <> (S.fromCharArray $ A.replicate (18-len) '0') <> w
  where dropZeros = S.fromCharArray ∘ A.reverse ∘ (A.dropWhile (\c → c == '0')) ∘ A.reverse ∘ S.toCharArray

type AddrLookupFn = ∀ e. (Array StringAddr → Eff e Unit) → StringId → Eff e Unit
type SingleAddrLookupFn = ∀ e. (StringAddr → Eff e Unit) → StringId → Eff e Unit
type AddrComparisonFn = ∀ e. (Boolean → Eff e Unit) → StringAddr → StringAddr → Eff e Unit
type NameLookupFn = ∀ e. (StringId → Eff e Unit) → StringAddr → Eff e Unit
type StringNumLookupFn = ∀ e. (StringNum → Eff e Unit) → StringId → Eff e Unit
type NumberLookupFn = ∀ e. (Number → Eff e Unit) → StringId → Eff e Unit

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
foreign import depositWeiImpl ∷ ∀ e. StringId → StringNum → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import withdrawDepositImpl ∷ ∀ e. StringId → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import getDepositWeiImpl  ∷ StringNumLookupFn
foreign import expirationDateImpl ∷ NumberLookupFn

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
    Just fi → liftEff $ depositWeiImpl ((fnGetName ∘ fiGetName) fi) (weiStr w)

withdrawDeposit ∷ MonadF Unit
withdrawDeposit = do
  mId ← foundationId
  case mId of
    Nothing → throwError NoFoundationId
    Just fi → liftEff $ withdrawDepositImpl ((fnGetName ∘fiGetName) fi)

getDepositWei ∷ MonadF (Maybe Wei)
getDepositWei = do
  mId ← foundationId
  case mId of
    Nothing → pure Nothing
    Just i  → (Just ∘ mkWei) <$>
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
