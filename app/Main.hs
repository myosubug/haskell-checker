module Main where

--import Tui

import Checkers
import qualified GameLogic as GL
import qualified CheckersAI_ABPruning as R



main :: IO ()
--main = redAi red_ai apply_move initialGameState
main = aiTest R.red_ai R.black_ai R.apply_move GL.initialGameState

