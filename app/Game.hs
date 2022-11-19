{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Random
import System.Random (Random(..), newStdGen)
import System.IO.Unsafe
import System.Random (newStdGen, randomRs)
type BoardSize = Int
type JewelSize = Int
type JewelVal = Int

data Difficulty = Easy | Medium | Hard
  deriving (Eq, Show)

data Block = Block
    {
        val::JewelVal,
        pos::Pos
    }
    deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 0 <= pRow < dim 
  , pCol :: Int  -- 0 <= pCol < dim
  }
  deriving (Eq, Ord, Show)

type Board = NonEmptyCursor Row

type Row = NonEmptyCursor Block

data State = State
    {
        board::Board,
        score::Int
    }
    deriving (Eq, Show)

type ResourceName = String

testBoard :: [[Block]]
testBoard = initBoardValue 3 3

initBoardValue :: BoardSize -> JewelSize -> [[Block]]
initBoardValue bsize gsize = [
    [Block {val = 4, pos = (getPos 0 0)}, Block {val = 2, pos = (getPos 0 1)}, Block {val = 3, pos = (getPos 0 1)}],
    [Block {val = 4, pos = (getPos 1 0)}, Block {val = 4, pos = (getPos 1 1)}, Block {val = 4, pos = (getPos 1 2)}],
    [Block {val = 4, pos = (getPos 2 0)}, Block {val = 8, pos = (getPos 2 1)}, Block {val = 1, pos = (getPos 2 2)}]]


getPos::Int -> Int -> Pos
getPos col row = Pos{ pRow=col, pCol=row}

initBoard :: BoardSize -> JewelSize -> Board -- TODO: generate board by bsize and gsize
initBoard bsize gsize = setBoardState (initBoardValue bsize gsize)

drawState :: State -> [Widget ResourceName] -- TODO: also show score, etc.
drawState st = drawBoard (board st)

drawBoard :: Board -> [Widget ResourceName]
drawBoard bd = 
    [vBox $
    concat
        [map drawRow $ reverse $ nonEmptyCursorPrev bd,
        [drawRow $ nonEmptyCursorCurrent bd],
        map drawRow $ nonEmptyCursorNext bd
        ]]

drawRow :: Row -> Widget n
drawRow blks = 
    hBox $ 
    concat
        [ map drawBlock $ reverse $ nonEmptyCursorPrev blks
        , [drawBlock $ nonEmptyCursorCurrent blks]
        , map drawBlock $ nonEmptyCursorNext blks
        ]

drawBlock :: Block -> Widget n -- TODO: show selected block with highlight (and with figures instead of ints)
drawBlock blk = str (show (val blk) ++ "  ")

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
                EvKey (KChar 'r') [] -> continue $ State {board = setBoardState ((removeBlock (getBoardList (board(s))))), score = 0}
                EvKey (KChar 'a') [] -> continue $ State {board = setBoardState ((addNewBlocks (getBoardList (board(s))))), score = 0}
                EvKey (KChar 'q') [] -> halt s -- TODO: may change exit key (q)
                -- TODO: capture keyboard/mouse event
                _ -> continue s
        _ -> continue s


cancelBlocks :: State -> State
cancelBlocks = error "TODO"

mapOnBoard :: (a -> b) -> [[a]] -> [[b]]
mapOnBoard f' board = map (map f') board

removeBlock :: [[Block]] -> [[Block]]
removeBlock board = mapOnBoard f' board 
          where f' b  = if ifBlockElimiable board (pRow (pos(b))) (pCol (pos(b)))
                            then Block {val = -1, pos = pos(b)}
                            else b


addNewBlocks :: [[Block]]-> [[Block]]
addNewBlocks board = mapOnBoard f' board
          where f' b = if val (b) == -1
                            then Block {val = 1, pos = pos(b)}
                            else b


ifBlockElimiable :: [[Block]] -> Int -> Int -> Bool
ifBlockElimiable board row column = matchInRow board row column
    || matchInRow (transpose board) column row


matchInRow :: [[Block]] -> Int -> Int -> Bool
matchInRow board row column = 
    (isSameBlk board row (column-1) row column) && (isSameBlk board row column row (column+1))
    || (isSameBlk board row (column-1) row (column-2)) && (isSameBlk board row column row (column-1))
    || (isSameBlk board row (column+1) row (column+2)) && (isSameBlk board row column row (column+1))


isSameBlk :: [[Block]] -> Int -> Int -> Int -> Int-> Bool
isSameBlk board r1 c1 r2 c2 = (val (getBlock board r1 c1 )) == (val (getBlock board r2 c2))


getBlock :: [[Block]] -> Int -> Int -> Block
getBlock board row column
    | (column < 0 || row < 0 || row >= (length board) || column >= (length (board!!0))) = Block {val = -1}
    | otherwise = (board !! row) !! column


nonEmptyCursorToList :: NonEmptyCursor a -> [a]
nonEmptyCursorToList s = NE.toList (rebuildNonEmptyCursor s)


listToNonEmptyCursor :: [a] -> NonEmptyCursor a
listToNonEmptyCursor l = case NE.nonEmpty l of
                                Just r -> makeNonEmptyCursor r
                                Nothing -> error "empty"


getBoardList :: Board -> [[Block]]
getBoardList s = map (\row -> nonEmptyCursorToList row) (nonEmptyCursorToList s)


setBoardState :: [[Block]] -> Board
setBoardState board_list = listToNonEmptyCursor (map listToNonEmptyCursor board_list)
                                

