module Main where

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
pickLevel :: IO Game.Difficulty
pickLevel = error "TODO"

-- Show score and ask for restart or exit
handleEndGame :: Game.State -> IO ()
handleEndGame b = error "TODO"


