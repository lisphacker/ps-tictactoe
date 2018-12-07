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

import Data.Array ((!!))

import Game

import Debug.Trace

data Query a = ResetGame a
             | Play Int a

--data OutputMessage = Current State

type State = { gameState :: GameState
             , statusText :: String
             }

genButton :: forall p. Int -> GameState -> HH.HTML p (Query Unit)
genButton pos (GameState {cells, player}) = case cells !! pos of
                                              Just (Just p) -> HH.button [] [HH.text $ show p]
                                              Just Nothing  -> HH.button
                                                               [HE.onClick (HE.input_ (Play pos))]
                                                               [HH.text "*"]
                                              Nothing       -> HH.button [] [HH.text "*"]

myBoard :: forall m. H.Component HH.HTML Query Unit Void m
myBoard =
  H.component { initialState: const initialState
              , render
              , eval
              , receiver: const Nothing
              }
  where

    initialState :: State
    initialState = {gameState: newGame, statusText: ""}

    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = show state
      in HH.div []
         [ HH.button [HE.onClick (HE.input_ ResetGame)] [HH.text "Reset"]
         , HH.table []
           [
             HH.tr []
             [
               HH.td []
               [ genButton 0 state.gameState
               , genButton 1 state.gameState
               , genButton 2 state.gameState
               ]
             ]
           , HH.tr []
             [
               HH.td []
               [ genButton 3 state.gameState
               , genButton 4 state.gameState
               , genButton 5 state.gameState
               ]
             ]
           , HH.tr []
             [
               HH.td []
               [ genButton 6 state.gameState
               , genButton 7 state.gameState
               , genButton 8 state.gameState
               ]
             ]
           ]
         , HH.text state.statusText
         ]

    eval :: Query ~> (H.ComponentDSL State Query Void m)
    eval = case _ of
      ResetGame qp -> do
        state <- H.get
        let nextState = {gameState: newGame, statusText: ""}
        H.put nextState
        --H.raise $ Current nextState
        pure qp

      Play pos qp -> do
        state <- H.get
        let nextState = case play pos state.gameState of
              Just state' -> {gameState: switchPlayer state', statusText: ""}
              Nothing     -> {gameState: state.gameState, statusText: "Invalid move"}
        H.put nextState
        --H.raise $ Current nextState
        pure $ trace (show nextState) (\_ -> qp)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI myBoard unit body
