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
import Data.Array as A

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
import Foundation.Config       as C
import Foundation.Blockchain   (handleCall, hasNetworkError)
import UI.UIStatesKit          as UIStates

import Data.Array as A

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | MainViewMsg MainView.Message a
  | RefreshMetamask a
  | PreviousScreen a
  | SetScreen R.Screen a

type State = { loggedIn ∷ Boolean
             , errorBus ∷ ContainerMsgBus
             , txs      ∷ Array E.TX
             , currentScreen ∷ R.Screen
             , history  ∷ Array R.Screen
             , myId     ∷ Maybe F.FoundationId
             , myAddr   ∷ Maybe E.EthAddress }

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
                   , errorBus: Nothing
                   , txs: []
                   , currentScreen: R.OverviewScreen
                   , history: []
                   , myId: Nothing
                   , myAddr: Nothing }

    render ∷ State → H.ParentHTML Query ChildQuery ChildSlot (AppMonad eff)
    render state =
      HH.div [ HP.id_ "container",
               HP.class_ (HH.ClassName $
                 "container " <>
                 (R.getRouteNameFor state.currentScreen)) ]
      [
        topBar state
      , HH.div [ HP.id_ "body" ]
        [
          HH.slot' CP.cp1 unit MainView.component
          { msgBus: state.errorBus, myId: state.myId }
          $ HE.input MainViewMsg
        ]
      , menu state.currentScreen state.myId
      ]

    eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad eff)
    eval = case _ of
      Init next → do
        bus ← H.liftAff $ Bus.make
        H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
        H.liftEff $ UIStates.toggleLoading(".container")
        H.modify (_ { loggedIn = true, errorBus = Just bus })
        loadWeb3Loop C.web3Delay 10
        H.liftEff $ UIStates.toggleLoading(".container")
        startCheckInterval (Just bus) C.checkMMInterval C.checkTxInterval
        pure next
      HandleMsg msg next →
        case msg of
          (FoundationError fe) → do
            handleFoundationError fe
            pure next
          NetworkError → do
            hLog NetworkError
            H.modify (_ { loggedIn = false})
            pure next
          CheckMetamask → do
            checkMetamask
            pure next
          CheckTxs  → do
            txs ← H.gets _.txs
            bus ← H.gets _.errorBus
            statii ← H.liftAff $ sequence (MM.checkTxStatus <$> txs)
            if hasNetworkError statii
              then case bus of
                Nothing → pure unit
                Just b  → H.liftAff $ Bus.write NetworkError b
              else do
                let pending = A.filter (\(Tuple s _) → E.notDone s) $ A.zip statii txs
                if A.length pending /= A.length txs
                  then do
                    H.modify (_ { txs = (\(Tuple _ tx) → tx) <$> pending })
                    refreshMetamask
                  else pure unit
            pure next
      RefreshMetamask next → do
        refreshMetamask
        pure next
      SetScreen screen next → do
        H.modify (\s → s { history = append [s.currentScreen] s.history })
        H.modify (_ { currentScreen = screen  })
        pure next
      MainViewMsg msg next →
        case msg of
          MainView.ScreenChange screen → do
            H.modify (\s → s { history = append [s.currentScreen] s.history })
            H.modify (_ { currentScreen = screen  })
            pure next
          MainView.NewTX newTx → do
            H.modify (\s → s { txs = s.txs <> [newTx] })
            pure next
      PreviousScreen next → do
        H.modify (\state → state {currentScreen = (fromMaybe R.OverviewScreen $ A.head state.history), history = (fromMaybe [] $ A.tail state.history)})
        pure next

handleFoundationError fError =
  case fError of
    F.NoMetamask → do
      hLog fError
      H.modify (_ { loggedIn = false })
    F.NoFoundationId → do
      hLog fError
      H.modify (_ { myId = Nothing })
    F.TxError → do
      hLog "Transaction not completed."
    F.NameInUse → do
      hLog fError
    F.AddrInUse → do
      hLog fError
    _  → do
      hLog fError

topBar ∷ ∀ p. State → H.HTML p Query
topBar state =
  let numTxs     = A.length state.txs
      processing = numTxs /= 0
      itemStr    = if numTxs == 1 then "item" else "items"
  in
    HH.div [ HP.class_ (HH.ClassName "row top-bar")]
    [
      HH.div [ HP.class_ (HH.ClassName "col logo-section")]
      [
        HH.img [HP.src "Foundation@1x.png"],
        HH.text "Foundation Manager"
      ]
      , HH.div [HP.class_ (HH.ClassName "col go-back-section")]
        [
          HH.a [HP.href "#", HP.class_ (HH.ClassName "close-pop-button"), HE.onClick $ HE.input_ $ PreviousScreen]
          [HH.i [ HP.class_ (HH.ClassName "fa fa-chevron-left")][], HH.text " Back"]
        ]
      , HH.div [HP.class_ (HH.ClassName $ "col-4 align-self-end current-transactions" <> if processing then " processing" else "") ]
        [
          HH.i [HP.class_ (HH.ClassName "transaction-spinner")][],
          HH.span_ [HH.text $
                    "Writing " <> show (A.length state.txs) <> " " <> itemStr <> "..."]
        ]
    ]

promptMetamask ∷ ∀ p. Boolean → H.HTML p Query
promptMetamask overlayInactive =
  HH.div [ HP.id_ "metamaskOverlay"
         , if overlayInactive then HP.class_ (HH.ClassName "in-active")
           else HP.class_ (HH.ClassName "active")]
  [
    HH.h6_ [ HH.text "Not logged in to Metamask." ]
    , HH.button [ HE.onClick $ HE.input_ $ RefreshMetamask
                , HP.class_ $ HH.ClassName "btn-info"]
      [ HH.i [HP.class_ (HH.ClassName "fa fa-refresh")][] ]
  ]

--check for loggedIn changes and user address changes
refreshMetamask ∷ ∀ e. H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad e) Unit
refreshMetamask = do
  mmLoggedIn ← H.liftEff MM.loggedIn
  myAddr     ← H.liftEff MM.currentUserAddress
  H.modify (_ { myAddr = Just myAddr })
  if mmLoggedIn
    then do _ ← H.query' CP.cp1 unit (MainView.ReloadAll unit)
            H.modify (_ { loggedIn = true })
    else do H.modify (_ { loggedIn = false })

checkMetamask ∷ ∀ e. H.ParentDSL State Query ChildQuery ChildSlot Void (AppMonad e) Unit
checkMetamask = do
  mmStatus ← H.liftEff MM.loggedIn
  loggedIn ← H.gets _.loggedIn
  myAddr   ← H.gets _.myAddr
  if (loggedIn && mmStatus)
    then do
      newAddr ← Just <$> (H.liftEff MM.currentUserAddress)
      if isNothing myAddr || myAddr /= newAddr
        then refreshMetamask
        else pure unit
    else refreshMetamask


startCheckInterval maybeBus mmInterval txInterval = do
  case maybeBus of
    Nothing → pure unit
    Just bus  → do
      _ ← H.liftEff $ setInterval mmInterval $ checkMMEff  bus
      _ ← H.liftEff $ setInterval txInterval $ checkTxsEff bus
      pure unit
      where checkMMEff b = do
              _ ← launchAff $ Bus.write CheckMetamask b
              pure unit
            checkTxsEff b = do
              _ ← launchAff $ Bus.write CheckTxs b
              pure unit

-- view Components
menu ∷ ∀ p. R.Screen → Maybe F.FoundationId → H.HTML p Query
menu currentScreen myId =
  case myId of
    Nothing →
      HH.div
        [ HP.class_ (HH.ClassName "header-menu col")]
        []
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

--keep checking if metamask is loaded
loadWeb3Loop delayMs numTriesLeft = do
  b ← H.gets _.errorBus
  case b of
    Nothing  → pure unit
    Just bus →
      if numTriesLeft > 0
      then do
        hLog "Retrying"
        H.liftAff $ delay (Milliseconds (toNumber delayMs))
        mmLoggedIn ← H.liftEff MM.loggedIn
        if mmLoggedIn
          then do
            myId ← handleCall (Just bus) Nothing FoundationError F.foundationId
            H.modify (_ { myId = myId })
            refreshMetamask
          else loadWeb3Loop delayMs (numTriesLeft - 1)
      else do
        H.liftAff $ Bus.write NetworkError bus
        pure unit
