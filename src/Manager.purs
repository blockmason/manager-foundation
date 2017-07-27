module Foundation.Manager where

import Foundation.Prelude

import Foundation.Types                (ContainerMsg(..), ContainerMsgBus, AppMonad)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array as A
import Data.String as S
import Data.Int as I
import Data.DateTime                   (DateTime(..))
import Data.Formatter.DateTime as DTF

import Foundation.Blockchain           (handleFCall)
import Foundation.Routes as R
import Network.Eth.Foundation  as F
import Network.Eth             as E

data Query a
  = RefreshAddress a
  | ReloadAll a
  | HandleInput Input a
  | GoToPage R.Screen a
  | ConfirmUnification F.FoundationName a
  | InputNewAddress String a
  | InputNewName String a
  | CreateNewId String a
  | AddNewAddress (Either String E.EthAddress) a
  | ExtendId a
  | InputFundAmount String a
  | FundId E.Wei a

type State = { loading          ∷ Boolean
             , errorBus         ∷ ContainerMsgBus
             , myId             ∷ Maybe F.FoundationId
             , addresses        ∷ Array E.EthAddress
             , sentPending      ∷ Maybe E.EthAddress
             , todoPending      ∷ Maybe F.FoundationName
             , expiryDate       ∷ Maybe DateTime
             , funds            ∷ Maybe E.Wei
             , newAddress       ∷ Either String E.EthAddress
             , newName          ∷ String
             , fundAmountWei    ∷ E.Wei
             , weiToExtend      ∷ E.Wei
             }

--type ScreenChange = R.Screen
data Message
  = ScreenChange R.Screen
  | NewTX        E.TX
type Input = { msgBus ∷ ContainerMsgBus, myId ∷ Maybe F.FoundationId }

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
                       , errorBus: input.msgBus
                       , myId: input.myId
                       , addresses: []
                       , sentPending: Nothing
                       , todoPending: Nothing
                       , expiryDate: Nothing
                       , funds: Nothing
                       , newAddress: Left ""
                       , newName: ""
                       , fundAmountWei: E.zeroWei
                       , weiToExtend:   E.zeroWei
                     }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "main-view")]
      [
        page R.OverviewScreen (summary state.myId state.expiryDate (A.length state.addresses) state.funds)
      , page R.ManageAddressesScreen
          (addressesPage state.addresses state.sentPending state.todoPending)
      , page R.AddAddressScreen (addAddressPage state)
      , page R.RegisterScreen (createIdPage state)
      , page R.ExtendIDScreen (extendIdPage state.expiryDate state.weiToExtend)
      , page R.FundIDScreen (fundsPage state)
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Message (AppMonad eff)
  eval = case _ of
    HandleInput input next → do
      H.modify (_ { errorBus = input.msgBus, myId = input.myId })
      pure next
    ReloadAll next → do
      s ← H.get
      loadFromBlockchain (s.myId)
      pure next
    RefreshAddress next → do
      pure next
    GoToPage route next → do
      H.raise $ ScreenChange route
      pure next
    ConfirmUnification name next → do
      handleTx $ F.confirmPendingUnification name
      pure next
    InputNewAddress addrs next → do
      if ((S.length addrs) == 42) --
        then H.modify (_ { newAddress = Right $ E.EthAddress addrs })
        else H.modify (_ { newAddress = Left addrs })
      pure next
    AddNewAddress eitherAddr next → do
      case eitherAddr of
        Left _      → pure next
        Right addr  → do
          H.modify (_ { newAddress = Left "" })
          handleTx $ F.addPendingUnification addr
          pure next
    ExtendId next → do
      handleTx $ F.extendIdOneYear
      pure next
    InputFundAmount strWei next → do
      hLog $ E.mkWei strWei
      H.modify (_ { fundAmountWei = E.mkWei strWei })
      pure next
    FundId weiAmount next → do
      H.modify (_ { loading = true })
      handleTx $ F.depositWei weiAmount
      H.modify (_ { loading = false })
      pure next
    InputNewName nameStr next → do
      H.modify (_ { newName = nameStr })
      pure next
    CreateNewId name next → do
      handleTx $ F.createId $ F.fnMkName name
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

summary ∷ Maybe F.FoundationId → Maybe DateTime → Int → Maybe E.Wei
        → H.ComponentHTML Query
summary optionalID expiryDate addressCount funds =
  let balance = fromMaybe (E.zeroWei) funds
  in case optionalID of
    Nothing →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ "No ID found. Please register, or confirm an address in \"Manage Addresses\"")
        ]
    Just (F.FoundationId myId) →
      HH.div
        [HP.class_ (HH.ClassName "col myid-summary")]
        [
          (card "ID" $ HH.text $ show myId.name),
          (card "Expires" $ HH.text $ maybe "" formatDate expiryDate ),
          (card "Addresses" $ HH.text $ show addressCount ⊕ " associated"),
          (card "Current Deposit" $ HH.text $ E.weiShowEth balance ⊕ " Eth" )
        ]

addressesPage ∷ Array E.EthAddress → Maybe E.EthAddress → Maybe F.FoundationName
              → H.ComponentHTML Query
addressesPage addresses sentPending todoPending =
      HH.div
        [HP.class_ (HH.ClassName "col address-list")] $
        [
          (card "" $
           HH.button [ HE.onClick $ HE.input_ $ GoToPage R.AddAddressScreen
                     , HP.class_ $ HH.ClassName "confirm-address-button"]
           [ HH.text "Add Address" ])
        ] ⊕
        (maybe []
         (\sp → [(card "Waiting for confirmation from:" $ HH.text $ show sp)])
         sentPending)
        ⊕
        (maybe []
         (\tp → [(card "Confirmation required for id:" $ idConfirmation tp)])
         todoPending)
        ⊕
        ((\address → card "Unified Address" $ HH.text $ show address) <$> addresses)
  where idConfirmation fName =
          HH.div [ HP.class_ (HH.ClassName "col") ]
          [ HH.text $ show fName
          , HH.button [ HE.onClick $ HE.input_ $ ConfirmUnification fName
                      , HP.class_ $ HH.ClassName "btn btn-secondary"]
            [ HH.text "Confirm Address Link" ]
          ]

addAddressRequestBlock ∷ F.FoundationName → H.ComponentHTML Query
addAddressRequestBlock name =
  HH.div
    [HP.class_ (HH.ClassName "")]
    [HH.button [ HE.onClick $ HE.input_ $ ConfirmUnification name
                  , HP.class_ $ HH.ClassName "confirm-unification-button"]
                  [ HH.text $ "Combine with: " ⊕ show name]]

addAddressPage ∷ State → H.ComponentHTML Query
addAddressPage state =
  HH.div
    [HP.class_ (HH.ClassName "col add-address-page")]
    [
      (card ("Link a new address to " ⊕ (maybe "" (show ∘ F.fiGetName) state.myId)) $
       HH.div
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
                    , HP.disabled $ isLeft state.newAddress
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
          [ HH.text "Add Address" ]
        ]
      )
    ]

extendIdPage ∷ Maybe DateTime → E.Wei → H.ComponentHTML Query
extendIdPage expiryDate weiToExtend =
  HH.div
    [HP.class_ (HH.ClassName "col extend-id-page")]
    [
      (card "Expires" $ HH.text $ maybe "" formatDate expiryDate )
    , (card "Extend for 1 Year" $
       HH.button [ HE.onClick $ HE.input_ $ ExtendId
                 , HP.class_ $ HH.ClassName "btn btn-secondary"]
       [ HH.text $ "Extend for " ⊕ E.weiShowEth weiToExtend ⊕ " ETH" ])
    ]

fundsPage ∷ State → H.ComponentHTML Query
fundsPage state =
  HH.div
    [HP.class_ (HH.ClassName "col funds-page")]
    [
      (card "Balance" $ HH.text $ (maybe "" show state.funds) ⊕ " Wei"),
      (card ("Deposit: " ⊕ E.weiShowEth state.fundAmountWei ⊕ " Eth") $
       HH.div [ HP.class_ (HH.ClassName "col") ]
       [ HH.input [ HP.type_ HP.InputText
                  , HP.class_ $ HH.ClassName "row"
                  , HP.placeholder $ "Enter wei to deposit here"
                  , HP.value $ E.weiStr state.fundAmountWei
                  , HE.onValueInput
                    (HE.input (\val → InputFundAmount val))
                  ]
       , HH.button [ HE.onClick $ HE.input_ $ FundId state.fundAmountWei
                   , HP.class_ $ HH.ClassName "btn btn-secondary" ]
         [ HH.text "Deposit ETH" ]
        ])
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
                   , HP.placeholder $ "Enter an id"
                   , HE.onValueInput
                     (HE.input (\val → InputNewName val))
                   ]
        , HH.button [ HE.onClick $ HE.input_ $ CreateNewId state.newName
                    , HP.class_ $ HH.ClassName "btn btn-secondary"]
          [ HH.text "Create Foundation ID" ]
        ]
      )
    ]

loadFromBlockchain myId = do
  eb ← H.gets _.errorBus
  H.modify (_ { loading = true })
  sentPending ← handleFCall eb Nothing F.sentPending
  todoPending ← handleFCall eb Nothing F.todoPending
  expiryDate  ← handleFCall eb Nothing F.expirationDate
  depWei      ← handleFCall eb Nothing F.getDepositWei
  weiToExtend ← handleFCall eb E.zeroWei F.getWeiToExtend
  let addrs = maybe [] F.fiGetAddrs myId
  H.modify (_ { loading = false, addresses = addrs
              , sentPending = sentPending, todoPending = todoPending
              , funds = depWei, expiryDate = expiryDate
              , weiToExtend = weiToExtend })

formatDate ∷ DateTime → String
formatDate = (either (const "") id) ∘ (DTF.formatDateTime "YYYY-MM-DD")

watchTx tx = if E.isBlank tx then pure unit else H.raise $ NewTX tx

handleTx f = do
  s ← H.get
  tx ← handleFCall s.errorBus E.blankTx f
  watchTx tx
  H.raise $ ScreenChange R.OverviewScreen
