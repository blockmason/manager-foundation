module Network.Eth where

import Prelude
import Data.String                 as S
import Data.Array                  as A
import Data.Int                    (round, toNumber, fromString)
import Data.Maybe                  (Maybe(..), maybe, isJust)

infixr 9 compose as ∘
infixr 5 append as ⊕

type StringAddr = String
type StringId = String
type StringNum = String
type StringHash = String

type RawTx = { txHash ∷ StringHash }
rawToTX ∷ RawTx → TX
rawToTX rawTx = (TX ∘ eaMkAddr) rawTx.txHash

data TX = TX EthAddress
instance showTX ∷ Show TX where
  show (TX e) = "Transaction: " ⊕ show e
blankTx = TX $ eaMkAddr ""

newtype EthAddress = EthAddress StringAddr
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = S.localeCompare ua1 ua2
getEa ∷ EthAddress → String
getEa (EthAddress ea) = ea
eaMkAddr = EthAddress
isNull ∷ EthAddress → Boolean
isNull (EthAddress ea) = ea == "0x0000000000000000000000000000000000000000"

newtype Wei = Wei String
mkWei ∷ String → Wei
mkWei ""  = Wei "0"
mkWei str = if isInt str then Wei str else zeroWei
  where isInt "" = true
        isInt s  = (isJust (fromString $ S.take 9 s)) && (isInt $ S.drop 9 s)
zeroWei ∷ Wei
zeroWei = Wei "0"
instance showWei ∷ Show Wei where
  show (Wei s) = s
weiGet ∷ Wei → String
weiGet (Wei s) = s
weiStr ∷ Wei → String
weiStr (Wei s) = s

weiShowEth ∷ Wei → String
weiShowEth (Wei w) =
  let len = S.length w
  in if len > 18
     then dropZeros $ S.take (len-18) w <> "." <> S.drop (len-18) w
     else dropZeros $ "0." <> (S.fromCharArray $ A.replicate (18-len) '0') <> w
  where dropZeros s =
          let rev = (A.dropWhile (\c → c == '0')) ∘ A.reverse ∘ S.toCharArray $ s
          in if A.head rev == (Just '.')
             then S.fromCharArray ∘ A.reverse ∘ (A.drop 1) $ rev
             else S.fromCharArray ∘ A.reverse $ rev
