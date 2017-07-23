module Foundation.Routes where

import Prelude

data Screen =
    OverviewScreen
  | ManageAddressesScreen
  | AddAddressScreen
  | RegisterScreen
  | ExtendIDScreen
  | FundIDScreen

getRouteNameFor ∷ Screen → String
getRouteNameFor screen =
  case screen of
    OverviewScreen →
      "show-overview-screen"
    ManageAddressesScreen →
      "show-manage-addresses-screen"
    AddAddressScreen →
      "show-add-address-screen"
    RegisterScreen →
      "show-register-screen"
    ExtendIDScreen →
      "show-extend-id-screen"
    FundIDScreen →
      "show-fund-id-screen"

getContainerNameFor ∷ Screen → String
getContainerNameFor screen =
  case screen of
    OverviewScreen →
      "overview-screen-container"
    ManageAddressesScreen →
      "manage-addresses-screen-container"
    AddAddressScreen →
      "add-address-screen-container"
    RegisterScreen →
      "register-screen-container"
    ExtendIDScreen →
      "extend-id-screen-container"
    FundIDScreen →
      "fund-id-screen-container"

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
