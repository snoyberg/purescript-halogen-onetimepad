module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Control.Monad.Eff.Random

import Encrypt (encrypt)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)

main :: Eff (HA.HalogenEffects (arrayBuffer :: ARRAY_BUFFER, random :: RANDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI encrypt unit body
