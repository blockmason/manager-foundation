module Network.Eth.Foundation
       (
         FOUNDATION
       , FoundationId(..)
       , FoundationName(..)
       , EthAddr(..)
       , Error(..)
       , StringAddr
       , StringId
       , runMonadF
       , idByAddr
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(..))
import Data.String                 (localeCompare)
import Network.Eth.Metamask        (loggedIn, currentUserAddress, METAMASK)

infixr 9 compose as ∘
foreign import data FOUNDATION ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (foundation ∷ FOUNDATION, metamask ∷ METAMASK | e)) a
runMonadF = runExceptT

type DummyVal = String
type StringAddr = String
type StringId = String

data Error = NoMetamask
instance showError ∷ Show Error where
  show NoMetamask = "NoMetamask: Metamask not logged in."

newtype EthAddr = EthAddr StringAddr
instance showEthAddr ∷ Show EthAddr where
  show (EthAddr ua) = ua
instance eqEthAddr ∷ Eq EthAddr where
  eq (EthAddr ua1) (EthAddr ua2) = ua1 == ua2
instance ordEthAddr ∷ Ord EthAddr where
  compare (EthAddr ua1) (EthAddr ua2) = localeCompare ua1 ua2
getUa ∷ EthAddr → String
getUa (EthAddr ua) = ua

newtype FoundationName = FoundationName StringId
instance showFoundationName ∷ Show FoundationName where
  show (FoundationName fn) = fn

newtype FoundationId = FoundationId { name      ∷ FoundationName
                                    , addresses ∷ Array EthAddr }

type AddrLookupFn = ∀ e. (Array StringAddr → Eff e Unit) → StringId → Eff (foundation ∷ FOUNDATION | e) Unit
type NameLookupFn = ∀ e. (StringId → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. DummyVal → Eff (foundation ∷ FOUNDATION | e) Unit
foreign import resolveToAddrImpl ∷ AddrLookupFn
foreign import resolveToNameImpl ∷ NameLookupFn

checkAndInit ∷ MonadF Unit
checkAndInit = do
  li ← liftEff loggedIn
  if li
    then liftEff $ initImpl ""
    else throwError NoMetamask

currentUser ∷ MonadF EthAddr
currentUser = do
  checkAndInit
  EthAddr <$> liftEff currentUserAddress

{-
idByName ∷ FoundationName → MonadF FoundationId
idByName = do
-}

idByAddr ∷ EthAddr → MonadF FoundationName
idByAddr (EthAddr ea) = do
  checkAndInit
  FoundationName <$> (liftAff $ makeAff (\err succ → resolveToNameImpl succ ea))



--"0x6c48110d0f02814f5b27ab7dc9734d69494389f4"
