{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

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

type Board = NonEmptyCursor Row

type Row = NonEmptyCursor Block

data State = State
    {
        board::Board,
        score::Int
    }
    deriving (Eq, Show)

type ResourceName = String

initBoard :: BoardSize -> JewelSize -> Board -- TODO: generate board by bsize and gsize
initBoard bsize gsize = case NE.nonEmpty [Block {val = 1}, Block {val = 2}, Block {val = 3}] of
                            Just r1 ->
                                case NE.nonEmpty [Block {val = 4}, Block {val = 5}, Block {val = 6}] of
                                    Just r2 -> 
                                        case NE.nonEmpty [Block {val = 7}, Block {val = 8}, Block {val = 9}] of
                                            Just r3 ->
                                                case NE.nonEmpty [makeNonEmptyCursor r1, makeNonEmptyCursor r2, makeNonEmptyCursor r3] of 
                                                    Just output -> makeNonEmptyCursor output
                                                    Nothing -> error "empty in initBoard"
                                            Nothing -> error "empty in initBoard"
                                    Nothing -> error "empty in initBoard"
                            Nothing -> error "empty in initBoard"

drawState :: State -> [Widget ResourceName] -- TODO: also show score, etc.
drawState st = drawBoard (board st)

drawBoard :: Board -> [Widget ResourceName]
drawBoard bd = [vBox $ map drawRow bd] -- TODO: fix for NonEmptyCursor
-- For example,
-- [vBox $ concat
--         [map drawRow $ reverse $ nonEmptyCursorPrev bd,
--          [drawRow $ nonEmptyCursorCurrent bd],
--          map drawRow $  nonEmptyCursorNext bd
--         ]
-- ]

drawRow :: Row -> Widget n
drawRow blks = vBox $ map drawBlock blks -- TODO: fix for NonEmptyCursor

drawBlock :: Block -> Widget n -- TODO: show selected block with highlight (and with figures instead of ints)
drawBlock blk = str (show (val blk))

initGame :: Difficulty -> IO State
initGame diff = let iBoard = initBoard 3 3 in
                let iState = State {board = iBoard, score = 0} in
                do 
                   _ <- defaultMain jLApp iState
                   return iState

playGame :: State -> IO ()
playGame b = error "TODO"

jLApp :: App State e ResourceName
jLApp = App 
        {
            appDraw = drawState,
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleSelectEvent,
            appStartEvent = pure,
            appAttrMap = const $ attrMap mempty []
        }

handleSelectEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleSelectEvent s e =
    case e of 
        VtyEvent vtye -> 
            case vtye of
                EvKey (KChar 'q') [] -> halt s -- TODO: may change exit key (q)
                -- TODO: capture keyboard/mouse event
                _ -> continue s
        _ -> continue s
