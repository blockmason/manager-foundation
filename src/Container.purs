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
import Foundation.Manager as MainView
import Foundation.Routes as R

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | RefreshMetamask a
  | SetScreen String a
  | ShowPreviousScreen a

type Message = String
type State = { loggedIn ∷ Boolean
             , loading  ∷ Boolean
             , errorBus ∷ ContainerMsgBus
             , currentScreen ∷ String
             , previousScreen ∷ String}

type ChildQuery = Coproduct1 MainView.Query
type ChildSlot = Either1 Unit

ui ∷ ∀ eff. H.Component HH.HTML Query Unit Void (AppMonad eff)
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

    initialState ∷ State
    initialState = { loggedIn: true
                   , loading: true
                   , errorBus: Nothing
                   , currentScreen: "show-overview-screen"
                   , previousScreen: "show-overview-screen" }

    render ∷ State → H.ParentHTML Query ChildQuery ChildSlot (AppMonad eff)
    render state =
      HH.div [ HP.id_ "container", HP.class_ (HH.ClassName $ "container-fluid " <> state.currentScreen) ]
      [ promptMetamask state.loggedIn
      , loadingOverlay state.loading
      , HH.div [ HP.id_ "back-nav-bar", HP.class_ (HH.ClassName "row back-nav-bar")]
        [
          HH.a [HP.href "#", HP.class_ (HH.ClassName "close-pop-button"), HE.onClick $ HE.input_ $ ShowPreviousScreen]
          [HH.i [ HP.class_ (HH.ClassName "fa fa-chevron-left")][], HH.text " Back"]
        ]
      , HH.div [ HP.id_ "body" ]
        [
          HH.slot' CP.cp1 unit MainView.component state.errorBus $ HE.input SetScreen
        ]
      , menu state.currentScreen
      ]

    eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad eff)
    eval = case _ of
      Init next → do
        bus ← H.liftAff $ Bus.make
        H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
        H.modify (_ { loggedIn = true, loading = true, errorBus = Just bus })
        H.liftAff $ delay (Milliseconds (toNumber 1500))
        H.modify (_ { loading = false })
        refreshMetamask
        (H.liftAff $ F.runMonadF $ F.foundationId) >>= hLog
--        (H.liftAff $ F.runMonadF $ F.currentAddr) >>= hLog
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
      SetScreen className next → do
        H.modify (\state → state {previousScreen = state.currentScreen})
        H.modify (_ {currentScreen = className})
        pure next
      ShowPreviousScreen next → do
        H.modify (\state → state {currentScreen = state.previousScreen})
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

-- view Components
menu ∷ ∀ p. String → H.HTML p Query
menu currentScreen =
  HH.div
    [ HP.class_ (HH.ClassName "header-menu col")]
    [
        menuItem R.ManageAddressesScreen currentScreen
      , menuItem R.AddAddressScreen currentScreen
      , menuItem R.RegisterScreen currentScreen
      , menuItem R.ExtendIDScreen currentScreen
      , menuItem R.FundIDScreen currentScreen
    ]

menuItem ∷ ∀ p. R.Screen → String → H.HTML p Query
menuItem screen currentScreen =
  HH.a
  [HP.href "#",
        HP.class_ (HH.ClassName $ "row " <> if currentScreen == (R.getRouteNameFor screen) then "active" else ""),
        HE.onClick $ HE.input_ $ SetScreen (R.getRouteNameFor screen)]
  [ HH.text $ R.getMenuNameFor screen]
