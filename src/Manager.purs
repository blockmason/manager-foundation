module Foundation.Manager where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as A
import Data.String as S

import Foundation.Routes as R
import Network.Eth.Foundation as F

data Query a
  = RefreshAddress a
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
             , sentUnification  ∷ Array F.PendingUnification
             , todoUnification  ∷ Array F.FoundationName
             , expiryDate       ∷ String
             , funds            ∷ F.Wei
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
                       , addresses: mockEthAddesses
                       , sentUnification: mockSentUnification
                       , todoUnification: mockTodoUnification
                       , expiryDate: randomDate
                       , funds: F.Wei 10000.0
                       , newAddress: Left ""
                       , newName: ""
                     }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
          page R.OverviewScreen (summary state.myId state.expiryDate (A.length state.addresses) state.funds)
        , page R.ManageAddressesScreen (addressesPage state.addresses state.sentUnification state.todoUnification)
        , page R.AddAddressScreen (addAddressPage state)
        , page R.RegisterScreen (createIdPage state)
        , page R.ExtendIDScreen (extendIdPage state.expiryDate)
        , page R.FundIDScreen (fundsPage state)
      ]

  eval ∷ Query ~> H.ComponentDSL State Query ScreenChange (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      pure next
    RefreshAddress next → do
      pure next
    GoToPage route next → do
      H.raise route
      pure next
    ConfirmUnification name next → do
      pure next
    InputNewAddress addrs next → do
      if ((S.length addrs) > 41) --
        then H.modify (_ { newAddress = Right $ F.EthAddress addrs })
        else H.modify (_ { newAddress = Left addrs })
      pure next
    AddNewAddress addrs next → do
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

summary ∷ Maybe F.FoundationId → String → Int → F.Wei → H.ComponentHTML Query
summary optionalID expiryDate addressCount balance=
  case optionalID of
    Nothing →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ "No ID found. Please register...")
        ]
    Just (F.FoundationId myId) →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ show myId.name),
          (card "Expires" $ HH.text randomDate),
          (card "Addresses" $ HH.text $ show addressCount <> " associated"),
          (card "Current Balance" $ HH.text $ show balance <> " Wei")
        ]

addressesPage ∷ Array F.EthAddress → Array F.PendingUnification → Array F.FoundationName → H.ComponentHTML Query
addressesPage addresses pendingUnifications todoUnifications =
      HH.div
        [HP.class_ (HH.ClassName "col address-list")]
        $ append [
          (card "" $
            HH.button [ HE.onClick $ HE.input_ $ GoToPage R.AddAddressScreen
                      , HP.class_ $ HH.ClassName "confirm-address-button"]
                      [ HH.text "Add Address" ])
        ]
        $ append ((\(F.FoundationId pending) → card "Waiting for confirmation from:" $ HH.text $ show pending.addrs) <$> pendingUnifications)
        $ append ((\name → card "Combine address request:" $ addAddressRequestBlock name) <$> todoUnifications)
        $ ((\address → card "" $ HH.text $ show address) <$> addresses)

addAddressRequestBlock ∷ F.FoundationName → H.ComponentHTML Query
addAddressRequestBlock name =
  HH.div
    [HP.class_ (HH.ClassName "")]
    [HH.button [ HE.onClick $ HE.input_ $ ConfirmUnification name
                  , HP.class_ $ HH.ClassName "confirm-unification-button"]
                  [ HH.text $ "Combine with: " <> show name]]

addAddressPage ∷ State → H.ComponentHTML Query
addAddressPage state =
  HH.div
    [HP.class_ (HH.ClassName "col add-address-page")]
    [
      (card "Add new address" $  HH.div
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
      (card "Balance" $ HH.text $ show state.funds <> " Wei"),
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
