module Foundation.Address where

import Foundation.Prelude

import Types (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = RefreshAddress a
  | HandleInput Input a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
             }

type Input = ContainerMsgBus

component ∷ ∀ eff. H.Component HH.HTML Query Input Void (AppMonad eff)
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
    HH.div_ [ HH.text "Address" ]

  eval ∷ Query ~> H.ComponentDSL State Query Void (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      pure next
    RefreshAddress next → do
      pure next
