module Foundation.Types where

import Foundation.Prelude
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.Eth.Metamask (MetamaskStatus(..), METAMASK)
import Network.Eth.Foundation      as F
import Network.Eth            (EthAddress)

------------------- App Monad(s) ---------------------------
type AppMonad eff = (Aff (exception ∷ EXCEPTION, timer ∷ TIMER, random ∷ RANDOM, avar ∷ AVAR, console ∷ CONSOLE, ajax ∷ AJAX, metamask ∷ METAMASK, foundation ∷ F.FOUNDATION | eff))

------------------- App State -----------------------------
data ContainerMsg
  = FoundationError F.FoundationError
  | CheckMetamask

instance showContainerMsg ∷ Show ContainerMsg where
  show (FoundationError fe) = "FoundationError: " ⊕ show fe
  show CheckMetamask        = "Checking Metamask status."

type ContainerMsgBus = Maybe (Bus.BusRW ContainerMsg)
