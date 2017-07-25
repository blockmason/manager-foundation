module Foundation.Manager where

import Foundation.Prelude

import Foundation.Types                (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as A
import Data.String as S
import Data.DateTime                   (DateTime(..))

import Foundation.Blockchain           (handleFCall)
import Foundation.Routes as R
import Network.Eth.Foundation as F

data Query a
  = RefreshAddress a
  | ReloadAll a
  | HandleInput Input a
  | GoToPage R.Screen a
  | ConfirmUnification F.FoundationName a
  | InputNewAddress String a
  | InputNewName String a
  | CreateNewId String a
  | AddNewAddress (Either String F.EthAddress) a
  | ExtendId a
  | FundId a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
             , myId             ∷ Maybe F.FoundationId
             , addresses        ∷ Array F.EthAddress
             , sentPending      ∷ Maybe F.EthAddress
             , todoPending      ∷ Maybe F.FoundationName
             , expiryDate       ∷ String
             , funds            ∷ Maybe F.Wei
             , newAddress       ∷ Either String F.EthAddress
             , newName          ∷ String
             }

type ScreenChange = R.Screen
type Input = ContainerMsgBus

component ∷ ∀ eff. H.Component HH.HTML Query Input ScreenChange (AppMonad eff)
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
                       , errorBus: input
                       , myId: Nothing
                       , addresses: []
                       , sentPending: Nothing
                       , todoPending: Nothing
                       , expiryDate: randomDate
                       , funds: Nothing
                       , newAddress: Left ""
                       , newName: ""
                     }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
          page R.OverviewScreen (summary state.myId state.expiryDate (A.length state.addresses) state.funds)
        , page R.ManageAddressesScreen
          (addressesPage state.addresses state.sentPending state.todoPending)
        , page R.AddAddressScreen (addAddressPage state)
        , page R.RegisterScreen (createIdPage state)
        , page R.ExtendIDScreen (extendIdPage state.expiryDate)
        , page R.FundIDScreen (fundsPage state)
      ]

  eval ∷ Query ~> H.ComponentDSL State Query ScreenChange (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      H.modify (_ { errorBus = input } )
      pure next
    ReloadAll next → do
      s ← H.get
      loadFromBlockchain
      pure next
    RefreshAddress next → do
      pure next
    GoToPage route next → do
      H.raise route
      pure next
    ConfirmUnification name next → do
      errorBus ← H.gets _.errorBus
      handleFCall errorBus unit $ F.confirmPendingUnification name
      pure next
    InputNewAddress addrs next → do
      if ((S.length addrs) == 42) --
        then H.modify (_ { newAddress = Right $ F.EthAddress addrs })
        else H.modify (_ { newAddress = Left addrs })
      pure next
    AddNewAddress eitherAddr next → do
      errorBus ← H.gets _.errorBus
      case eitherAddr of
        Left _      → pure next
        Right addr  → do
          handleFCall errorBus unit $ F.addPendingUnification addr
          pure next
    ExtendId next → do
      pure next
    FundId next → do
      pure next
    InputNewName nameStr next → do
      H.modify (_ { newName = nameStr })
      pure next
    CreateNewId name next → do
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

summary ∷ Maybe F.FoundationId → String → Int → Maybe F.Wei → H.ComponentHTML Query
summary optionalID expiryDate addressCount funds =
  let balance = fromMaybe (F.mkWei 0.0) funds
  in case optionalID of
    Nothing →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ "No ID found. Please register, or confirm an address in \"Manage Addresses\"")
        ]
    Just (F.FoundationId myId) →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ show myId.name),
          (card "Expires" $ HH.text randomDate),
          (card "Addresses" $ HH.text $ show addressCount ⊕ " associated"),
          (card "Current Deposit" $ HH.text $ show balance ⊕ " Wei" )
        ]

addressesPage ∷ Array F.EthAddress → Maybe F.EthAddress → Maybe F.FoundationName
              → H.ComponentHTML Query
addressesPage addresses sentPending todoPending =
      HH.div
        [HP.class_ (HH.ClassName "col address-list")] $
        [
          (card "" $
           HH.button [ HE.onClick $ HE.input_ $ GoToPage R.AddAddressScreen
                     , HP.class_ $ HH.ClassName "confirm-address-button"]
           [ HH.text "Add Address" ])
        ] ⊕
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
                   , HP.class_ $ HH.ClassName "row"
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

extendIdPage ∷ String → H.ComponentHTML Query
extendIdPage expiryDate =
  HH.div
    [HP.class_ (HH.ClassName "col extend-id-page")]
    [
      (card "Expires" $ HH.text expiryDate),
      (card "Extend for 1 Year" $
        HH.button [ HE.onClick $ HE.input_ $ ExtendId
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
                  [ HH.text "Extend for 0.1 ETH" ])
    ]

fundsPage ∷ State → H.ComponentHTML Query
fundsPage state =
  HH.div
    [HP.class_ (HH.ClassName "col funds-page")]
    [
      (card "Balance" $ HH.text $ show state.funds ⊕ " Wei"),
      (card "Deposit" $
        HH.button [ HE.onClick $ HE.input_ $ FundId
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
                  [ HH.text "Deposit ETH" ])
    ]

createIdPage ∷ State → H.ComponentHTML Query
createIdPage state =
  HH.div
    [HP.class_ (HH.ClassName "col create-id-page")]
    [
      (card "Make new id" $  HH.div
        [HP.class_ (HH.ClassName "col")]
        [
          HH.input [ HP.type_ HP.InputText
                   , HP.value $ ""
                   , HP.class_ $ HH.ClassName "row"
                   , HP.placeholder $ "some_name"
                   , HE.onValueInput
                     (HE.input (\val → InputNewName val))
                   ]
        , HH.button [ HE.onClick $ HE.input_ $ CreateNewId state.newName
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
          [ HH.text "Create Foundation ID" ]
        ]
      )
    ]

loadFromBlockchain = do
  eb ← H.gets _.errorBus
  H.modify (_ { loading = true })
  myId        ← handleFCall eb Nothing F.foundationId
  sentPending ← handleFCall eb Nothing F.sentPending
  todoPending ← handleFCall eb Nothing F.todoPending
  expiryDate  ← handleFCall eb Nothing F.expirationDate
  depWei      ← handleFCall eb Nothing F.getDepositWei
  hLog expiryDate
  let addrs = fromMaybe [] (F.fiGetAddrs <$> myId)
  H.modify (_ { myId = myId, loading = false, addresses = addrs
              , sentPending = sentPending, todoPending = todoPending
              , funds = depWei })

-- mocks
randomDate ∷ String
randomDate = "2018-11-01"

mockFoundationName1 ∷ F.FoundationName
mockFoundationName1 = F.FoundationName "Luke"

mockFoundationName2 ∷ F.FoundationName
mockFoundationName2 = F.FoundationName "Tim"

mockEthAddesses ∷ Array F.EthAddress
mockEthAddesses = [F.EthAddress "0x0", F.EthAddress "0x1"]

mockMe ∷ F.FoundationId
mockMe = (F.FoundationId {name: mockFoundationName1, addrs: mockEthAddesses})

mockTim ∷ F.FoundationId
mockTim = (F.FoundationId {name: mockFoundationName2, addrs: mockEthAddesses})

mockSentUnification ∷ Array F.PendingUnification
mockSentUnification = [mockTim]

mockTodoUnification ∷ Array F.FoundationName
mockTodoUnification = [mockFoundationName2]
