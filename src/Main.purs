module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Game

type State = Int

data Query a = Increment a
             | Decrement a

data Message = Updated Int

myButton :: forall m. H.Component HH.HTML Query Unit Message m
myButton =
  H.component { initialState: const initialState
              , render
              , eval
              , receiver: const Nothing
              }
  where

    initialState :: State
    initialState = 0

    --render :: State -> H.ComponentHTML Query () m
    --render :: State -> HH.HTML Void (Query Unit)
    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = show state
      in HH.div []
         [ HH.button
           [ HE.onClick (HE.input_ Increment) ]
           [ HH.text "+" ]
         , HH.text label
         , HH.button
           [ HP.enabled false, HE.onClick (HE.input_ Decrement) ]
           [ HH.text "-" ]
         ]

    --eval :: Query ~> H.HalogenM State Query () Message m
    eval :: Query ~> (H.ComponentDSL State Query Message m)
    eval = case _ of
      Increment next -> do
        state <- H.get
        let nextState = state + 1
        H.put nextState
        H.raise $ Updated nextState
        pure next
      Decrement next -> do
        state <- H.get
        let nextState = state - 1
        H.put nextState
        H.raise $ Updated nextState
        pure next

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI myButton unit body
