module Foundation.Blockchain where

import Foundation.Prelude
import Foundation.Types (ContainerMsg(..))
import Control.Monad.Aff.Bus as Bus
import Halogen as H
import Network.Eth.Foundation as F

--helper to query the blockchain
--blankVal is a value to return if there's an error
--writes a message to the error bus if there's an error
handleFCall errorBus blankVal affCall = do
  case errorBus of
    Nothing → do
      hLog "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftAff $ F.runMonadF affCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (FoundationError error) b
                        pure blankVal
        Right val  → pure val
