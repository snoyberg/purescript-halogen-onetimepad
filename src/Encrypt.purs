module Encrypt where

import Prelude

import Data.Maybe (Maybe (..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.String as S

import Data.TextEncoder (encodeUtf8)
import Data.ArrayBuffer.Types (Uint8Array, Uint8)
import Data.ArrayBuffer.Typed (toIntArray, unsafeAt)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER, byteLength)
import Data.ArrayBuffer.Show
import Data.Foldable (foldMap, length)

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random

data Query a = SetMessage String a

type State = { message :: String, clearBinary :: String, randomBinary :: String, resultBinary :: String }

encrypt :: forall eff. H.Component HH.HTML Query Unit Void
              (Aff (arrayBuffer :: ARRAY_BUFFER, random :: RANDOM | eff))
encrypt =
  H.component
    { initialState: const { message: "", clearBinary: "", randomBinary: "", resultBinary: "" }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Encrypt" ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.value state.message
          , HP.autofocus true
          , HE.onValueInput (HE.input SetMessage)
          ]
      , HH.p_
          [ HH.text "Clear text binary: "
          , HH.text state.clearBinary
          ]
      , HH.p_
          [ HH.text "Random binary: "
          , HH.text state.randomBinary
          ]
      , HH.p_
          [ HH.text "Result binary: "
          , HH.text state.resultBinary
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (arrayBuffer :: ARRAY_BUFFER, random :: RANDOM | eff))
  eval = case _ of
    SetMessage message next -> do
      clearBinary <- liftEff (bytesToBinary (encodeUtf8 message))
      randomBinary <- liftEff (randomBinaryString (S.length clearBinary))
      let resultBinary = xor clearBinary randomBinary
      H.modify (\_ ->
        { message: message
        , clearBinary: clearBinary
        , randomBinary: randomBinary
        , resultBinary: resultBinary
        })
      pure next

bytesToBinary :: forall eff. Uint8Array -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) String
bytesToBinary arr = do
  iarr <- toIntArray arr
  pure (foldMap byteToBinary iarr)
  {-
  loop "" 0
  where
    len = byteLength arr
    loop res idx
      | idx >= len = pure res
  -}

byteToBinary :: Int -> String
byteToBinary =
  loop "" 8
  where
    loop total 0 _ = total
    loop total remaining val =
      loop (digit <> total) (remaining - 1) (val / 2)
      where
        digit
          | val `mod` 2 == 0 = "0"
          | otherwise = "1"

randomBinaryString :: forall eff. Int -> Eff (random :: RANDOM | eff) String
randomBinaryString =
  loop ""
  where
    loop s 0 = pure s
    loop s cnt = do
      x <- randomBool
      loop ((if x then "1" else "0") <> s) (cnt - 1)

xor :: String -> String -> String
xor x y =
  loop "" 0
  where
    loop s i =
      case S.charAt i x of
        Nothing -> s
        Just x' ->
          case S.charAt i y of
            Nothing -> s
            Just y' -> loop (s <> combine x' y') (i + 1)

    combine '0' '0' = "0"
    combine '1' '1' = "0"
    combine _ _ = "1"