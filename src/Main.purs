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

data Query a = ResetGame
             | Play Int

data OutputMessage = Current GameState

myBoard :: forall m. H.Component HH.HTML Query Unit OutputMessage m
myBoard =
  H.component { initialState: const initialState
              , render
              , eval
              , receiver: const Nothing
              }
  where

    initialState :: GameState
    initialState = newGame

    render :: GameState -> H.ComponentHTML Query
    render state =
      let
        label = show state
      in HH.div []
         [ HH.button [] [ HH.text "Reset" ]
         , HH.table []
           [
             HH.tr []
             [
               HH.td []
               [ HH.button [] []
               , HH.button [] []
               , HH.button [] []
               ]
             ]
           , HH.tr []
             [
               HH.td []
               [ HH.button [] []
               , HH.button [] []
               , HH.button [] []
               ]
             ]
           , HH.tr []
             [
               HH.td []
               [ HH.button [] []
               , HH.button [] []
               , HH.button [] []
               ]
             ]
           ]
         ]

    eval :: Query ~> (H.ComponentDSL GameState Query OutputMessage m)
    eval = case _ of
      ResetGame -> do
        state <- H.get
        let nextState = newGame
        H.put nextState
        H.raise $ Current nextState
        pure nextState

      Play pos -> do
        state <- H.get
        let nextState = state
        H.put nextState
        H.raise $ Current nextState
        pure nextState

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI myBoard unit body
