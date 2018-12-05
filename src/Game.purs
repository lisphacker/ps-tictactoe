module Game ( Player(..)
            , GameState(..)
            , newGame
            , play)
       where

import Prelude
import Data.Tuple
import Data.Maybe (Maybe(..), isNothing)
import Data.Array (alterAt, elemIndex, replicate, updateAt, (!!))

data Player = X | O

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"

instance eqPlayer :: Eq Player where
  eq X X = true
  eq O O = true
  eq _ _ = false

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

data GameState = GameState { cells  :: Array (Maybe Player)
                           , player :: Player
                           }

instance showGameState :: Show GameState where
  show (GameState {cells, player}) = "<" <> show cells <> " " <> show player <> ">"

newGame :: GameState
newGame = GameState { cells: replicate 9 Nothing, player: X }

play :: Int -> GameState -> Maybe GameState
play i (GameState {cells, player})
  | isNothing (cells !! i) = Nothing
  | otherwise              = let maybeCells' = alterAt i setCell cells
                             in
                              case maybeCells' of
                                Just cells' -> Just $ GameState { cells: cells', player: nextPlayer player }
                                Nothing     -> Nothing
  where setCell :: Maybe Player -> Maybe (Maybe Player)
        setCell Nothing = Just $ Just player
        setCell _       = Nothing

winScore :: Int
winScore = 2048

drawScore :: Int
drawScore = 1024

win :: GameState -> Boolean
win (GameState { cells, player }) = allSame 0 1 2 ||
                                    allSame 3 4 5 ||
                                    allSame 6 7 8 ||
                                    allSame 0 3 6 ||
                                    allSame 1 4 7 ||
                                    allSame 2 5 8 ||
                                    allSame 0 4 8 ||
                                    allSame 2 4 6
  where allSame :: Int -> Int -> Int -> Boolean
        allSame i j k = let mci = cells !! i
                            mcj = cells !! j
                            mck = cells !! k
                        in
                         allSame' mci mcj mck
        allSame' :: Maybe (Maybe Player) -> Maybe (Maybe Player) -> Maybe (Maybe Player) -> Boolean
        allSame' (Just (Just pi)) (Just (Just pj)) (Just (Just pk)) = pi == player &&
                                                                      pj == player &&
                                                                      pk == player
        allSame' _                _                _                = false
                           
draw :: GameState -> Boolean
draw gs@(GameState { cells, player }) = case elemIndex Nothing cells of
                                          Just _  -> false
                                          Nothing -> not $ win gs
