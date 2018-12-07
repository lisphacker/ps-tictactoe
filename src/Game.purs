module Game {-( Player(..)
            , GameState(..)
            , draw
            , enumMoves
            , newGame
            , play
            , win)-}
       where

import Prelude
import Data.Tuple

import Data.Foldable (foldl)
import Data.Array (alterAt, elemIndex, filter, mapWithIndex, replicate, updateAt, zip, (!!))
import Data.Maybe (Maybe(..), isNothing, fromJust)

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
                                Just cells' -> Just $ GameState { cells: cells', player: player }
                                Nothing     -> Nothing
  where setCell :: Maybe Player -> Maybe (Maybe Player)
        setCell Nothing = Just $ Just player
        setCell _       = Nothing

switchPlayer :: GameState -> GameState
switchPlayer (GameState {cells, player}) = GameState {cells: cells, player: nextPlayer player}

winScore :: Int
winScore = 1024 * 1024

drawScore :: Int
drawScore = 0 --1024

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

enumMoves :: GameState -> Array Int
enumMoves (GameState { cells, player }) = filter (\x -> x /= -1) $ mapWithIndex f cells
  where f i (Just _) = -1
        f i Nothing  = i

evaluate :: Int -> GameState -> Tuple Int Int
evaluate depth gs@(GameState { cells, player }) = let moves = enumMoves gs
                                                      scores = map computeScore moves
                                                  in maxScoredMove moves scores
  where computeScore :: Int -> Int
        computeScore move = case maybeComputeScore move of
                              Just score -> score
                              Nothing    -> 0
                              
        maybeComputeScore :: Int -> Maybe Int
        maybeComputeScore move = do
          gs' <- play move gs
          let score = if win gs' then
                        winScore
                      else
                        if draw gs' then
                          drawScore
                        else
                          let Tuple move' score' = evaluate (depth + 1) (switchPlayer gs')
                          in
                           -(score' / 2)
          pure score
          
        maxScoredMove :: Array Int -> Array Int -> Tuple Int Int
        maxScoredMove moves scores = foldl f (Tuple (-1) (-winScore - 1)) (zip moves scores)
          where f max@(Tuple _ maxScore) this@(Tuple _ s) = if s > maxScore then this else max

g0 = Just newGame
g1 = map switchPlayer $ g0 >>= play 0
g2 = map switchPlayer $ g1 >>= play 2 
g3 = map switchPlayer $ g2 >>= play 8
g4 = map switchPlayer $ g3 >>= play 4
g5 = map switchPlayer $ g4 >>= play 6
g6 = map switchPlayer $ g5 >>= play 3

g7 = map switchPlayer $ g6 >>= play 7
