module Main where

import Game

main :: IO ()
main = do
          welcome                                   -- Welcome message
          diff <- pickLevel                         -- pick difficulty level
          Game.initGame diff                        -- play game
          handleEndGame                        -- show score and options for restart

-- Show welcome message
welcome :: IO ()
welcome = putStrLn "Welcome!"

-- Ask for difficulty
pickLevel :: IO Game.Difficulty
pickLevel = do
            putStrLn "Please select the difficulty level: (Easy/Medium/Hard)" 
            -- TODO: parsing input
            name <- getLine
            if name == "Easy" then return Game.Easy
            else if name == "Medium" then return Game.Medium
            else if name == "Hard" then return Game.Hard
            else pickLevel
            -- return Game.Easy

-- Show score and ask for restart or exit
handleEndGame :: IO ()
handleEndGame = do
            putStrLn "Well Done" 




