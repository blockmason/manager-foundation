module Foundation.Manager where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Foundation.Routes as R

data Query a
  = RefreshAddress a
  | HandleInput Input a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
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
      []

  eval ∷ Query ~> H.ComponentDSL State Query Message (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      pure next
    RefreshAddress next → do
      pure next

page :: R.Screen → H.ComponentHTML Query → H.ComponentHTML Query
page screen child =
  HH.div
    [HP.class_ (HH.ClassName $ R.getContainerNameFor screen)]
    [child]
