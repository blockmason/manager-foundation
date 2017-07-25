module Foundation.Container where

import Foundation.Prelude

import Foundation.Types (ContainerMsg(..), ContainerMsgBus, AppMonad)
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

import Network.Eth             as E
import Network.Eth.Metamask    as MM
import Network.Eth.Foundation  as F
import Foundation.Manager      as MainView
import Foundation.Routes       as R
import Foundation.Blockchain   (handleFCall)

import Data.Array as A

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | RefreshMetamask a
  | SetScreen R.Screen a
  | PreviousScreen a

type ScreenChange = R.Screen
type State = { loggedIn ∷ Boolean
             , loading  ∷ Boolean
             , errorBus ∷ ContainerMsgBus
             , txs      ∷ Array E.TX
             , currentScreen ∷ R.Screen
             , history ∷ Array R.Screen
             , myId ∷ Maybe F.FoundationId}

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
                   , txs: []
                   , currentScreen: R.OverviewScreen
                   , history: []
                   , myId: Nothing }

    render ∷ State → H.ParentHTML Query ChildQuery ChildSlot (AppMonad eff)
    render state =
      HH.div [ HP.id_ "container", HP.class_ (HH.ClassName $ "container-fluid " <> (R.getRouteNameFor state.currentScreen)) ]
      [ promptMetamask state.loggedIn
      , loadingOverlay state.loading
      , HH.div [ HP.id_ "home-bar", HP.class_ (HH.ClassName "row home-bar")]
        [
          HH.text "Foundation Manager"
        ]
      , HH.div [ HP.id_ "back-nav-bar", HP.class_ (HH.ClassName "row back-nav-bar")]
        [
          HH.a [HP.href "#", HP.class_ (HH.ClassName "close-pop-button"), HE.onClick $ HE.input_ $ PreviousScreen]
          [HH.i [ HP.class_ (HH.ClassName "fa fa-chevron-left")][], HH.text " Back"]
        ]
      , HH.div [ HP.id_ "body" ]
        [
          HH.slot' CP.cp1 unit MainView.component
          { msgBus: state.errorBus, txs: state.txs, foundationId: state.myId }
          $ HE.input SetScreen
        ]
      , menu state.currentScreen state.myId
      ]

    eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad eff)
    eval = case _ of
      Init next → do
        bus ← H.liftAff $ Bus.make
        H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
        H.modify (_ { loggedIn = true, loading = true, errorBus = Just bus })
        H.liftAff $ delay (Milliseconds (toNumber 1500))
        H.modify (_ { loading = false })
        runTests
        refreshMetamask
        myId        ← handleFCall (Just bus) Nothing F.foundationId
        H.modify (_ { myId = myId })
        startCheckInterval (Just bus) 5000
        pure next
      HandleMsg msg next → do
        case msg of
          (FoundationError fe) → do
            hLog $ show (FoundationError fe)
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
      SetScreen screen next → do
        H.modify (\state → state {history = append [state.currentScreen] state.history })
        H.modify (_ {currentScreen = screen})
        pure next
      PreviousScreen next → do
        H.modify (\state → state {currentScreen = (fromMaybe R.OverviewScreen $ A.head state.history), history = (fromMaybe [] $ A.tail state.history)})
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
    then do _ ← H.query' CP.cp1 unit (MainView.ReloadAll unit)
            H.modify (_ { loggedIn = mmStatus })
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
menu ∷ ∀ p. R.Screen → Maybe F.FoundationId → H.HTML p Query
menu currentScreen myId =
  case myId of
    Nothing →
      HH.div
        [ HP.class_ (HH.ClassName "header-menu col")]
        [
            menuItem R.RegisterScreen currentScreen
        ]
    Just _ →
      HH.div
        [ HP.class_ (HH.ClassName "header-menu col")]
        [
            menuItem R.ManageAddressesScreen currentScreen
          , menuItem R.AddAddressScreen currentScreen
          , menuItem R.ExtendIDScreen currentScreen
          , menuItem R.FundIDScreen currentScreen
        ]

menuItem ∷ ∀ p. R.Screen → R.Screen → H.HTML p Query
menuItem screen currentScreen =
  HH.a
  [HP.href "#",
        HP.class_ (HH.ClassName $ "row " <> if screen == currentScreen then "active" else ""),
        HE.onClick $ HE.input_ $ SetScreen screen]
  [ HH.text $ R.getMenuNameFor screen]


runTests = do
--  _ ← H.liftAff $ F.runMonadF F.printTransaction
  pure unit
--  (H.liftAff $ F.runMonadF F.getPendingUnification) >>= hLog
--  (H.liftAff $ F.runMonadF F.foundationId) >>= hLog
