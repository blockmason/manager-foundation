module Foundation.Manager where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
             , expiryDate       ∷ 
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
                       , errorBus: input }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
          page R.OverviewScreen (HH.text $ R.getContainerNameFor R.OverviewScreen)
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

summary ∷ H.ComponentHTML Query
summary

-- mocks
mockFoundationName1 ∷ F.FoundationName
mockFoundationName1 = F.FoundationName "Luke"

mockFoundationName2 ∷ F.FoundationName
mockFoundationName2 = F.FoundationName "Tim"

mockMe :: F.FoundationId
mockMe = (F.FoundationId mockFoundationName1)

mockTim :: F.FoundationId
mockTim = (F.FoundationId mockFoundationName2)
