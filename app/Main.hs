module Main where

import Game

main :: IO ()
main = do
          welcome                                   -- Welcome message
          diff <- pickLevel                         -- pick difficulty level
          Game.initGame diff                        -- play game
        --   handleEndGame game                        -- show score and options for restart

-- Show welcome message
welcome :: IO ()
welcome = putStrLn "Welcome!"

-- Ask for difficulty
pickLevel :: IO Game.Difficulty
pickLevel = putStrLn "Please select the difficulty level: (Easy/Medium/Hard)" >>
            -- TODO: parsing input
            return Game.Easy

-- Show score and ask for restart or exit
handleEndGame :: Game.State -> IO ()
handleEndGame b = error "TODO"


