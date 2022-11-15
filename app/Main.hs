module Main where

import Control.Monad (when)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)


import Prelude
import Game

main :: IO ()
main = do
          welcome                                   -- Welcome message
          diff <- pickLevel                         -- pick difficulty level
          game <- Game.initGame diff                -- play game
          handleEndGame game                        -- show score and options for restart

-- Show welcome message
welcome :: IO ()
welcome = error "TODO"

-- Ask for difficulty
pickLevel :: Game.Difficulty
pickLevel = error "TODO"

-- Show score and ask for restart or exit
handleEndGame :: Game.Board -> IO ()
handleEndGame b = error "TODO"


