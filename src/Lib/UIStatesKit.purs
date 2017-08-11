module UI.UIStatesKit where

import Foundation.Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Data.String as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type CssSelector = String

foreign import turnOnLoadingImpl :: forall e. CssSelector -> Eff e Unit
foreign import turnOffLoadingImpl :: forall e. CssSelector -> Eff e Unit
foreign import toggleLoadingImpl :: forall e. CssSelector -> Eff e Unit
foreign import toggleErrorImpl :: forall e. CssSelector -> Eff e Unit
foreign import clearAllErrorsImpl :: forall e. CssSelector -> Eff e Unit
foreign import clearAllLoadingImpl :: forall e. CssSelector -> Eff e Unit

turnOnLoading :: forall e. CssSelector -> Eff e Unit
turnOnLoading selector = turnOnLoadingImpl selector

turnOffLoading :: forall e. CssSelector -> Eff e Unit
turnOffLoading selector = turnOffLoadingImpl selector

toggleLoading :: forall e. CssSelector -> Eff e Unit
toggleLoading selector = toggleLoadingImpl selector

toggleError :: forall e. CssSelector -> Eff e Unit
toggleError selector = toggleErrorImpl selector

clearAllErrors :: forall e. Maybe CssSelector -> Eff e Unit
clearAllErrors maybeParentSelector =
  case maybeParentSelector of
    Nothing → do
      clearAllErrorsImpl ""
    Just selector → do
      clearAllErrorsImpl selector

clearAllLoading :: forall e. Maybe CssSelector -> Eff e Unit
clearAllLoading maybeParentSelector =
  case maybeParentSelector of
    Nothing → do
      clearAllLoadingImpl ""
    Just selector → do
      clearAllLoadingImpl selector
