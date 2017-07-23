module Foundation.Routes where

import Prelude

data Screen =
    OverviewScreen
  | ManageAddressesScreen
  | AddAddressScreen
  | RegisterScreen
  | ExtendIDScreen
  | FundIDScreen

instance eqScreen ∷ Eq Screen where
  eq screen1 screen2 = (getRouteNameFor screen1) == (getRouteNameFor screen2)

getBaseName ∷ Screen → String
getBaseName screen =
  case screen of
    OverviewScreen →
      "overview-screen"
    ManageAddressesScreen →
      "manage-addresses-screen"
    AddAddressScreen →
      "add-address-screen"
    RegisterScreen →
      "register-screen"
    ExtendIDScreen →
      "extend-id-screen"
    FundIDScreen →
      "fund-id-screen"

getRouteNameFor ∷ Screen → String
getRouteNameFor = (append "show-") <<< getBaseName

getContainerNameFor ∷ Screen → String
getContainerNameFor = (\x → append x "-container") <<< getBaseName

getMenuNameFor ∷ Screen → String
getMenuNameFor screen =
  case screen of
    OverviewScreen →
      "Overview"
    ManageAddressesScreen →
      "Manage Addresses"
    AddAddressScreen →
      "Add Address"
    RegisterScreen →
      "Register ID"
    ExtendIDScreen →
      "Extend Foundation ID"
    FundIDScreen →
      "Foundation ID Funds"
