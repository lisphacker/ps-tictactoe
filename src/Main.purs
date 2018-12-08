module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.Int (toNumber)

import CSS as CSS
import CSS.TextAlign as CSSTA
import CSS.VerticalAlign as CSSVA

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.CSS as HCSS
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

data Status = InProgress
            | YouWin
            | IWin
            | Draw
            | Invalid

derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
  show InProgress = ""
  show YouWin     = "You win !!!"
  show IWin       = "I win !!!"
  show Draw       = "Draw"
  show Invalid    = "INVALID STATE"
              
type State = { gameState :: GameState
             , status    :: Status
             }

{-
genCellView :: forall p. Int -> State -> HH.HTML p (Query Unit)
genCellView pos state = let GameState {cells, player} = state.gameState
                      in case cells !! pos of
                        Just (Just p) -> HH.button [] [HH.text $ show p]
                        Just Nothing  -> HH.button
                                         (if state.status == InProgress then [HE.onClick (HE.input_ (Play pos))] else [])
                                         [HH.text "*"]
                        Nothing       -> HH.button [] [HH.text "*"]
-}

cellSize = toNumber 100

genCellView :: forall p. Int -> State -> HH.HTML p (Query Unit)
genCellView pos state = let GameState {cells, player} = state.gameState
                            css = HCSS.style do
                               CSS.width $ CSS.px cellSize
                               CSS.height $ CSS.px cellSize
                               CSS.fontSize $ CSS.pt (toNumber 40)
                            div = case cells !! pos of
                              Just (Just p) -> HH.div [css] [HH.text $ show p]
                              Just Nothing  -> HH.div
                                               (if state.status == InProgress
                                                then
                                                  [css,HE.onClick (HE.input_ (Play pos))]
                                                else
                                                  [css])
                                               [HH.text " "]
                              Nothing       -> HH.div [] [HH.text " "]
                        in
                          HH.td [
                                  HCSS.style do
                                     CSS.border CSS.solid (CSS.px (toNumber 1)) CSS.black
                                     CSSTA.textAlign CSSTA.center
                                     CSSVA.verticalAlign CSSVA.Middle
                                ] [div]

myBoard :: forall m. H.Component HH.HTML Query Unit Void m
myBoard =
  H.component { initialState: const initialState
              , render
              , eval
              , receiver: const Nothing
              }
  where

    initialState :: State
    initialState = {gameState: newGame, status: InProgress}

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
               genCellView 0 state
             , genCellView 1 state
             , genCellView 2 state
             ]
           , HH.tr []
             [
               genCellView 3 state
             , genCellView 4 state
             , genCellView 5 state
             ]
           , HH.tr []
             [
               genCellView 6 state
             , genCellView 7 state
             , genCellView 8 state
             ]
           ]
         , HH.text $ show state.status
         ]

    eval :: Query ~> (H.ComponentDSL State Query Void m)
    eval = case _ of
      ResetGame qp -> do
        state <- H.get
        let nextState = {gameState: newGame, status: InProgress}
        H.put nextState
        --H.raise $ Current nextState
        pure qp

      Play pos qp -> do
        H.modify_ $ makePlay pos
        --H.raise $ Current nextState
        pure qp

      where makePlay :: Int -> State -> State
            makePlay pos state = case play pos state.gameState of
              Just gs2 -> if win gs2
                          then
                            {gameState: gs2, status: YouWin}
                          else
                            if draw gs2
                            then
                              {gameState: gs2, status: Draw}
                            else
                              let gs3 = switchPlayer gs2
                                  Tuple move _ = evaluate 0 gs3
                              in
                               case play move gs3 of
                                 Just gs4 -> if win gs4
                                             then
                                               {gameState: gs4, status: IWin}
                                             else
                                               if draw gs4
                                               then
                                                 {gameState: gs4, status: Draw}
                                               else
                                                 {gameState: switchPlayer gs4, status: InProgress}
                                 Nothing -> {gameState: gs3, status: Invalid}
              Nothing -> {gameState: state.gameState, status: Invalid}

                            

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI myBoard unit body
