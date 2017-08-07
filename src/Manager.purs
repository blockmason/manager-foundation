module Foundation.Manager where

import Foundation.Prelude

import Foundation.Types                (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as A
import Data.String as S
import Data.Int as I
import Data.DateTime                   (DateTime(..))
import Data.Formatter.DateTime as DTF

import Foundation.Blockchain        (handleCall, loadingOverlay, formatDate, handleTx)
import Foundation.Routes       as R
import Network.Eth.Foundation  as F
import Network.Eth             as E

data Query a
  = RefreshAddress a
  | ReloadAll a
  | HandleInput Input a
  | GoToPage R.Screen a
  | ConfirmUnification F.FoundationName a
  | InputNewAddress String a
  | InputNewName String a
  | CreateNewId String a
  | AddNewAddress (Either String E.EthAddress) a
  | ExtendId a
  | InputFundAmount String a
  | FundId E.Wei a
  | WithdrawDeposit a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
             , myId             ∷ Maybe F.FoundationId
             , addresses        ∷ Array E.EthAddress
             , sentPending      ∷ Maybe E.EthAddress
             , todoPending      ∷ Maybe F.FoundationName
             , expiryDate       ∷ Maybe DateTime
             , funds            ∷ Maybe E.Wei
             , newAddress       ∷ Either String E.EthAddress
             , newName          ∷ String
             , fundAmountWei    ∷ E.Wei
             , weiToExtend      ∷ E.Wei
             }

data Message
  = ScreenChange R.Screen
  | NewTX        E.TX
type Input = { msgBus ∷ ContainerMsgBus, myId ∷ Maybe F.FoundationId }

component ∷ ∀ eff. H.Component HH.HTML Query Input Message (AppMonad eff)
component =
  H.component
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where

  initialState ∷ Input → State
  initialState input = { loading: false
                       , errorBus: input.msgBus
                       , myId: input.myId
                       , addresses: []
                       , sentPending: Nothing
                       , todoPending: Nothing
                       , expiryDate: Nothing
                       , funds: Nothing
                       , newAddress: Left ""
                       , newName: ""
                       , fundAmountWei: E.zeroWei
                       , weiToExtend:   E.zeroWei
                     }

  render ∷ State → H.ComponentHTML Query
  render state =
    if state.loading
    then loadingOverlay state.loading
    else
      HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
        page R.OverviewScreen (summary state.todoPending state.myId state.expiryDate (A.length state.addresses) state.funds)
      , page R.ManageAddressesScreen
          (addressesPage state.addresses state.sentPending state.todoPending)
      , page R.AddAddressScreen (addAddressPage state)
      , page R.RegisterScreen (createIdPage state)
      , page R.ExtendIDScreen (extendIdPage state.expiryDate state.weiToExtend)
      , page R.FundIDScreen (fundsPage state)
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Message (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      H.modify (_ { errorBus = input.msgBus, myId = input.myId })
      pure next
    ReloadAll next → do
      s ← H.get
      hLog "Reloading"
      loadFromBlockchain (s.myId)
      pure next
    RefreshAddress next → do
      pure next
    GoToPage route next → do
      H.raise $ ScreenChange route
      pure next
    ConfirmUnification name next → do
      s ← H.get
      handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
        F.confirmPendingUnification name
      pure next
    InputNewAddress addrs next → do
      if ((S.length addrs) == 42) --
        then H.modify (_ { newAddress = Right $ E.EthAddress addrs })
        else H.modify (_ { newAddress = Left addrs })
      pure next
    AddNewAddress eitherAddr next → do
      s ← H.get
      case eitherAddr of
        Left _      → pure next
        Right addr  → do
          H.modify (_ { newAddress = Left "" })
          handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
            F.addPendingUnification addr
          pure next
    ExtendId next → do
      s ← H.get
      handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
        F.extendIdOneYear
      pure next
    InputFundAmount strWei next → do
      H.modify (_ { fundAmountWei = E.mkWei strWei })
      pure next
    FundId weiAmount next → do
      H.modify (_ { loading = true })
      s ← H.get
      handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
        F.depositWei weiAmount
      H.modify (_ { loading = false })
      pure next
    WithdrawDeposit next → do
      s ← H.get
      case s.funds of
        Just funds → do
          H.modify (_ { loading = true })
          handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
            F.withdrawDeposit funds
          H.modify (_ { loading = false })
          pure next
        Nothing → pure next
    InputNewName nameStr next → do
      if F.fiStrValidId (S.toLower nameStr) || S.length nameStr < 4
        then H.modify (_ { newName = S.toLower nameStr })
        else H.modify (_ { newName = "" })
      pure next
    CreateNewId name next → do
      s ← H.get
      H.modify (_ { newName = "" })
      handleTx NewTX s (ScreenChange R.OverviewScreen) FoundationError $
        F.createId $ F.fnMkName name
      pure next

page ∷ R.Screen → H.ComponentHTML Query → H.ComponentHTML Query
page screen child =
  HH.div
    [HP.class_ (HH.ClassName $ R.getContainerNameFor screen)]
    [child]

card ∷ String → H.ComponentHTML Query → H.ComponentHTML Query
card cardTitle child =
  case cardTitle of
    "" →
    HH.div
      [HP.class_ (HH.ClassName "card row card-inverse")]
      [
        HH.div
          [HP.class_ (HH.ClassName "card-block")]
          [child]
      ]
    someTitle →
      HH.div
        [HP.class_ (HH.ClassName "card row card-inverse")]
        [
          HH.div
            [HP.class_ (HH.ClassName "card-header")]
            [HH.h4 [HP.class_ (HH.ClassName "card-title")][HH.text cardTitle]]
          , HH.div
            [HP.class_ (HH.ClassName "card-block")]
            [child]
        ]

summary ∷ Maybe F.FoundationName → Maybe F.FoundationId → Maybe DateTime → Int → Maybe E.Wei
        → H.ComponentHTML Query
summary pendingConfirmation optionalID expiryDate addressCount funds =
  let balance = fromMaybe (E.zeroWei) funds
  in case optionalID of
    Nothing →
      case pendingConfirmation of
        Nothing →
          HH.div
            [HP.class_ (HH.ClassName "col myid-summary")]
            [
              (card "ID" $ HH.text $ "No ID found. Please register.")
            ]
        Just pendingName →
          HH.div
            [HP.class_ (HH.ClassName "col myid-summary")]
            [
              (card "Merge with existing id, or register." $ addAddressRequestBlock pendingName)
            ]
    Just (F.FoundationId myId) →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ show myId.name),
          (card "Expires" $ HH.text $ maybe "" formatDate expiryDate ),
          (card "Addresses" $ HH.text $ show addressCount ⊕ " associated"),
          (card "Current Deposit" $ HH.text $ E.weiShowEth balance ⊕ " Eth" )
        ]

addressesPage ∷ Array E.EthAddress → Maybe E.EthAddress → Maybe F.FoundationName
              → H.ComponentHTML Query
addressesPage addresses sentPending todoPending =
      HH.div
        [HP.class_ (HH.ClassName "col address-list")] $
        [
          (card "" $
           HH.button [ HE.onClick $ HE.input_ $ GoToPage R.AddAddressScreen
                     , HP.class_ $ HH.ClassName "confirm-address-button"]
           [ HH.text "Add Address" ])
        ]
        ⊕
        (maybe []
         (\sp → [(card "Waiting for confirmation from:" $ HH.text $ show sp)])
         sentPending)
        ⊕
        (maybe []
         (\tp → [(card "Confirmation required for id:" $ idConfirmation tp)])
         todoPending)
        ⊕
        ((\address → card "Unified Address" $ HH.text $ show address) <$> addresses)
  where idConfirmation fName =
          HH.div [ HP.class_ (HH.ClassName "col") ]
          [ HH.text $ show fName
          , HH.button [ HE.onClick $ HE.input_ $ ConfirmUnification fName
                      , HP.class_ $ HH.ClassName "btn btn-secondary"]
            [ HH.text "Confirm Address Link" ]
          ]

addAddressRequestBlock ∷ F.FoundationName → H.ComponentHTML Query
addAddressRequestBlock name =
  HH.div
    [HP.class_ (HH.ClassName "")]
    [HH.button [ HE.onClick $ HE.input_ $ ConfirmUnification name
                  , HP.class_ $ HH.ClassName "confirm-unification-button"]
                  [ HH.text $ "Combine with: " ⊕ show name]]

addAddressPage ∷ State → H.ComponentHTML Query
addAddressPage state =
  HH.div
    [HP.class_ (HH.ClassName "col add-address-page")]
    [
      (card ("Link a new address to " ⊕ (maybe "" (show ∘ F.fiGetName) state.myId)) $
       HH.div
        [HP.class_ (HH.ClassName "col")]
        [
          HH.input [ HP.type_ HP.InputText
                   , HP.value $ ""
                   , HP.class_ $ HH.ClassName ""
                   , HP.placeholder $ "0x0"
                   , HE.onValueInput
                     (HE.input (\val → InputNewAddress val))
                   ]
        , HH.button [ HE.onClick $ HE.input_ $ AddNewAddress state.newAddress
                    , HP.disabled $ isLeft state.newAddress
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
          [ HH.text "Add Address" ]
        ]
      )
    ]

extendIdPage ∷ Maybe DateTime → E.Wei → H.ComponentHTML Query
extendIdPage expiryDate weiToExtend =
  HH.div
    [HP.class_ (HH.ClassName "col extend-id-page")]
    [
      (card "Expires" $ HH.text $ maybe "" formatDate expiryDate )
    , (card "Extend for 1 Year" $
       HH.button [ HE.onClick $ HE.input_ $ ExtendId
                 , HP.class_ $ HH.ClassName "btn btn-secondary"]
       [ HH.text $ "Extend for " ⊕ E.weiShowEth weiToExtend ⊕ " ETH" ])
    ]

fundsPage ∷ State → H.ComponentHTML Query
fundsPage state =
  HH.div
    [HP.class_ (HH.ClassName "col funds-page")]
    [
      (card "Balance" $ HH.text $ (E.weiShowEth $ fromMaybe (E.zeroWei) state.funds) ⊕ " Eth")
    , (card ("Deposit: " ⊕ E.weiShowEth state.fundAmountWei ⊕ " Eth") $
       HH.div [ HP.class_ (HH.ClassName "col") ]
       [ HH.input [ HP.type_ HP.InputText
                  , HP.class_ $ HH.ClassName ""
                  , HP.placeholder $ "Enter wei to deposit here"
                  , HP.value $ E.weiStr state.fundAmountWei
                  , HE.onValueInput
                    (HE.input (\val → InputFundAmount val))
                  ]
       , HH.button [ HE.onClick $ HE.input_ $ FundId state.fundAmountWei
                   , HP.class_ $ HH.ClassName "btn btn-secondary" ]
         [ HH.text "Deposit ETH" ]
        ])
      , (card "Withdrawal" $
         HH.div [ HP.class_ $ HH.ClassName "col" ]
         [ HH.button [ HE.onClick $ HE.input_ $ WithdrawDeposit
                     , HP.class_ $ HH.ClassName "btn btn-secondary" ]
           [ HH.text "Withdraw All ETH" ]])
    ]

createIdPage ∷ State → H.ComponentHTML Query
createIdPage state =
  HH.div
    [HP.class_ (HH.ClassName "col create-id-page")]
    [
      (card "Register new id" $  HH.div
        [HP.class_ (HH.ClassName "col")]
        [
          HH.input [ HP.type_ HP.InputText
                   , HP.value $ state.newName
                   , HP.class_ $ HH.ClassName "row"
                   , HP.placeholder $ "Enter an id"
                   , HE.onValueInput
                     (HE.input (\val → InputNewName val))
                   ]
        , HH.button [ HE.onClick $ HE.input_ $ CreateNewId state.newName
                    , HP.class_ $ HH.ClassName "btn btn-secondary"
                    , HP.enabled $ F.fiStrValidId state.newName]
          [ HH.text "Create Foundation ID" ]
        ]
      )
    ]

loadFromBlockchain myId = do
  eb ← H.gets _.errorBus
  H.modify (_ { loading = true })
  sentPending ← handleCall eb Nothing FoundationError F.sentPending
  todoPending ← handleCall eb Nothing FoundationError F.todoPending
  expiryDate  ← handleCall eb Nothing FoundationError F.expirationDate
  depWei      ← handleCall eb Nothing FoundationError F.getDepositWei
  weiToExtend ← handleCall eb E.zeroWei FoundationError F.getWeiToExtend
  let addrs = maybe [] F.fiGetAddrs myId
  H.modify (_ { loading = false, addresses = addrs
              , sentPending = sentPending, todoPending = todoPending
              , funds = depWei, expiryDate = expiryDate
              , weiToExtend = weiToExtend })
