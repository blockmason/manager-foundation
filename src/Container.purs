module Foundation.Container where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)
import Data.Either.Nested (Either1)
import Control.Monad.Eff.Console (logShow)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff (delay, launchAff)
import Control.Monad.Eff.Timer (setInterval)
import Data.Time.Duration (Milliseconds(..))
import Data.Int (toNumber)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Halogen.Component.Utils (busEventSource)
import Halogen.Query.EventSource as ES

import Network.Eth.Metamask as MM
import Network.Eth.Foundation as F
import Foundation.Address as A

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | RefreshMetamask a

type State = { loggedIn ∷ Boolean
             , loading  ∷ Boolean
             , errorBus ∷ ContainerMsgBus }

type ChildQuery = Coproduct1 A.Query
type ChildSlot = Either1 Unit

ui :: ∀ eff. H.Component HH.HTML Query Unit Void (AppMonad eff)
ui =
  H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: (Just (H.action Init))
  , finalizer: Nothing
  }
  where

    initialState :: State
    initialState = { loggedIn: true
                   , loading: true
                   , errorBus: Nothing }

    render :: State → H.ParentHTML Query ChildQuery ChildSlot (AppMonad eff)
    render state =
      HH.div [ HP.id_ "container" ]
      [ promptMetamask state.loggedIn
      , loadingOverlay state.loading
      , HH.h1_ [ HH.text "FoundationID Manager" ]
      , HH.div [ HP.class_ (HH.ClassName "row")
               , HP.id_ "container" ]
        [
          HH.div_
          [
            HH.text "Container"
          , HH.slot' CP.cp1 unit A.component state.errorBus absurd
          ]
        ]
      ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad eff)
    eval = case _ of
      Init next → do
        bus ← H.liftAff $ Bus.make
        H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
        H.modify (_ { loggedIn = true, loading = true, errorBus = Just bus })
        H.liftAff $ delay (Milliseconds (toNumber 1500))
        H.modify (_ { loading = false })
        refreshMetamask
        f1 ← H.liftAff $ F.runMonadF $ F.idByAddr (F.EthAddress "0x6c48110d0f02814f5b27ab7dc9734d69494389f4")
        f2 ← H.liftAff $ F.runMonadF $ F.idByName (F.FoundationName "timgalebach")
        hLog f1
        hLog f2
        (H.liftAff $ F.runMonadF $ F.foundationId) >>= hLog
        startCheckInterval (Just bus) 5000
        pure next
      HandleMsg msg next → do
        case msg of
          FoundationError → do
            H.modify (_ { loggedIn = false })
            pure next
          CheckMetamask → do
            mmStatus ← H.liftEff MM.loggedIn
            loggedIn ← H.gets _.loggedIn
            checkMetamask loggedIn mmStatus
            pure next
      RefreshMetamask next → do
        refreshMetamask
        pure next

loadingOverlay ∷ ∀ p i. Boolean → H.HTML p i
loadingOverlay loading =
  HH.div [ HP.id_ "loadingOverlay"
         , if loading then HP.class_ (HH.ClassName "active")
           else HP.class_ (HH.ClassName "inActive")]
  [ HH.h6_ [ HH.text "Loading Metamask..." ]]

promptMetamask ∷ ∀ p. Boolean → H.HTML p Query
promptMetamask loggedIn =
  HH.div [ HP.id_ "metamaskOverlay"
         , if loggedIn then HP.class_ (HH.ClassName "inActive")
           else HP.class_ (HH.ClassName "active")]
  [ HH.div_
    [ HH.h6_ [ HH.text "Not logged in to Metamask." ]
    , HH.button [ HE.onClick $ HE.input_ $ RefreshMetamask
                , HP.class_ $ HH.ClassName "btn-info"]
      [ HH.text "Retry" ]]]

refreshMetamask ∷ ∀ e. H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad e) Unit
refreshMetamask = do
  mmStatus ← H.liftEff MM.loggedIn
  if mmStatus
    then do newmmStatus ← H.liftEff MM.loggedIn
            H.modify (_ { loggedIn = newmmStatus })
    else do H.modify (_ { loggedIn = mmStatus })

checkMetamask ∷ ∀ e. Boolean → Boolean
              → H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad e) Unit
checkMetamask loggedIn mmStatus =
  if (loggedIn && mmStatus) then pure unit else refreshMetamask


startCheckInterval maybeBus ms = do
  case maybeBus of
    Nothing → pure unit
    Just b  → do
      _ ← H.liftEff $ setInterval ms $ effToRun b
      pure unit
      where effToRun bus = do
              _ ← launchAff $ Bus.write CheckMetamask bus
              pure unit
