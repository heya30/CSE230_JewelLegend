{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core

type BoardSize = Int
type JewelSize = Int
type JewelVal = Int

data Difficulty = Easy | Medium | Hard
  deriving (Eq, Show)

data Block = Block
    {
        val::JewelVal
    }
    deriving (Eq, Show)

type Board = [[Block]]

data State = State
    {
        board::Board,
        score::Int
    }
    deriving (Eq, Show)

type ResourceName = String

initBoard :: BoardSize -> JewelSize -> Board
initBoard bsize gsize = [[Block {val = 1}, Block {val = 2}, Block {val = 3}],
                         [Block {val = 4}, Block {val = 5}, Block {val = 6}], 
                         [Block {val = 7}, Block {val = 8}, Block {val = 9}]]

drawBoard :: Board -> [Widget ResourceName]
drawBoard bd = [vBox $ map drawRow bd]

drawRow :: [Block] -> Widget n
drawRow blks = vBox $ map drawCol blks

drawCol :: Block -> Widget n
drawCol blk = str (show (val blk))

initGame :: Difficulty -> IO State
initGame diff = let iBoard = initBoard 3 3 in
                let iState = State {board = iBoard, score = 0} in -- TODO: use iState later
                do 
                   _ <- defaultMain jLApp iBoard
                   return iState

playGame :: State -> IO ()
playGame b = error "TODO"

jLApp :: App Board e ResourceName
jLApp = App 
        {
            appDraw = drawBoard,
            appChooseCursor = showFirstCursor,
            -- appHandleEvent = ,
            appStartEvent = pure,
            appAttrMap = const $ attrMap mempty []
        }
