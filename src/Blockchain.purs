module Foundation.Blockchain where

import Foundation.Prelude
import Foundation.Types                (ContainerMsg(..))
import Data.DateTime                   (DateTime(..))
import Data.Formatter.DateTime as DTF
import Data.Array              as A
import Control.Monad.Aff.Bus   as Bus
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Properties as HP
import Network.Eth             as E
import Network.Eth.Foundation  as F

--helper to query the blockchain
--blankVal is a value to return if there's an error
--writes a message to the error bus if there's an error
handleCall errorBus blankVal errorConstructor affCall = do
  case errorBus of
    Nothing → do
      hLog "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftAff $ F.runMonadF affCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (errorConstructor error) b
                        pure blankVal
        Right val  → pure val

setWatchTx message tx = if E.isBlank tx then pure unit else H.raise $ message tx

handleTx message state homeScreenQuery errorConstructor fnToRun = do
  tx ← handleCall state.errorBus E.blankTx errorConstructor fnToRun
  setWatchTx message tx
  H.raise homeScreenQuery

hasNetworkError ∷ Array E.TxStatus → Boolean
hasNetworkError = not ∘ A.null ∘ (A.filter E.hasError)

formatDate ∷ DateTime → String
formatDate = (either (const "") id) ∘ (DTF.formatDateTime "YYYY-MM-DD")

loadingOverlay ∷ ∀ p i. Boolean → H.HTML p i
loadingOverlay loading =
  HH.div [ HP.id_ "loadingOverlay"
         , if loading then HP.class_ (HH.ClassName "active")
           else HP.class_ (HH.ClassName "in-active")]
  [
    HH.i [HP.class_ (HH.ClassName "loading-spinner")][],
    HH.h6_ [ HH.text "Loading..." ]
  ]
