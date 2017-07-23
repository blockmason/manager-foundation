module Foundation.Manager where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as A

import Foundation.Routes as R
import Network.Eth.Foundation as F

data Query a
  = RefreshAddress a
  | HandleInput Input a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
             , myId             ∷ F.FoundationId
             , addresses        ∷ Array F.EthAddress
             , sentUnification  ∷ Array F.PendingUnification
             , todoUnification  ∷ Array F.FoundationName
             , expiryDate       ∷ String
             , funds            ∷ F.Wei
             }

type Message = String
type Input = ContainerMsgBus


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
                       , errorBus: input
                       , myId: mockMe
                       , addresses: mockEthAddesses
                       , sentUnification: mockSentUnification
                       , todoUnification: mockTodoUnification
                       , expiryDate: randomDate
                       , funds: F.Wei 10000.0}

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
          page R.OverviewScreen (summary state.myId state.expiryDate (A.length state.addresses) state.funds)
        , page R.ManageAddressesScreen (HH.text $ R.getContainerNameFor R.ManageAddressesScreen)
        , page R.AddAddressScreen (HH.text $ R.getContainerNameFor R.AddAddressScreen)
        , page R.RegisterScreen (HH.text $ R.getContainerNameFor R.RegisterScreen)
        , page R.ExtendIDScreen (HH.text $ R.getContainerNameFor R.ExtendIDScreen)
        , page R.FundIDScreen (HH.text $ R.getContainerNameFor R.FundIDScreen)
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Message (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      pure next
    RefreshAddress next → do
      pure next

page ∷ R.Screen → H.ComponentHTML Query → H.ComponentHTML Query
page screen child =
  HH.div
    [HP.class_ (HH.ClassName $ R.getContainerNameFor screen)]
    [child]

card ∷ String → H.ComponentHTML Query → H.ComponentHTML Query
card cardTitle child =
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

summary ∷ F.FoundationId → String → Int → F.Wei → H.ComponentHTML Query
summary (F.FoundationId myId) expiryDate addressCount balance=
  HH.div
    [HP.class_ (HH.ClassName "col myid-summary")]
    [
      (card "ID" $ HH.text $ show myId.name),
      (card "Expires" $ HH.text randomDate),
      (card "Addresses" $ HH.text $ show addressCount <> " associated"),
      (card "Current Balance" $ HH.text $ show balance <> " Wei")
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
