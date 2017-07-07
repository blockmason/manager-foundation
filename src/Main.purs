module Main where

import Foundation.Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Foundation.Container as Container

main = HA.runHalogenAff $ do
  body ← HA.awaitBody
  runUI Container.ui unit body
