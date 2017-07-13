module Network.Eth.Foundation
       (
         FoundationId(..)
       ) where

import Prelude
import Control.Monad.Eff         (Eff)
import Math                      (abs)
import Control.Monad.Eff.Class   (liftEff)
import Control.Monad.Aff         (Aff, makeAff)
import Data.Either               (Either(Left, Right))
import Data.Maybe                (Maybe(..))

infixr 9 compose as ∘

newtype UserAddress = UserAddress BProgAddress
instance showUserAddress ∷ Show UserAddress where
  show (UserAddress ua) = ua
instance eqUserAddress ∷ Eq UserAddress where
  eq (UserAddress ua1) (UserAddress ua2) = ua1 == ua2
instance ordUserAddress ∷ Ord UserAddress where
  compare (UserAddress ua1) (UserAddress ua2) = localeCompare ua1 ua2
getUa ∷ UserAddress → String
getUa (UserAddress ua) = ua

newtype FoundationId = FoundationId { name      ∷ String
                                    , addresses ∷ Array UserAddress }

--foreign import resolveToAddrImpl
--foreign import resolveToNameImpl
