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
initBoard bsize gsize = error "TODO"

showBoard :: Board -> [Widget ResourceName]
showBoard bd = [vBox $ map drawRow bd]

drawRow :: [Block] -> Widget n
drawRow blks = hBox $ map drawCol blks

drawCol :: Block -> Widget n
drawCol blk = str (show (val blk))

initGame :: Difficulty -> IO State
initGame diff = error "TODO"

playGame :: State -> IO ()
playGame b = error "TODO"
