{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import Data.List
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

data Direction = DirUp | DirDown | DirLeft | DirRight
    deriving (Eq, Show)

data Block = Block
    {
        val::JewelVal
    }
    deriving (Eq, Show)


type Board = [Row]

type Row = [Block]

data State = State
    {
        board::Board,
        score::Int,
        selected::Bool,
        row::Int,
        col::Int
    }
    deriving (Eq, Show)

type ResourceName = String

testBoard :: [[Block]]
testBoard = initBoardValue 3 3

initBoardValue :: BoardSize -> JewelSize -> [[Block]]
initBoardValue bsize gsize = [
    [Block {val = 4}, Block {val = 2}, Block {val = 3}],
    [Block {val = 4}, Block {val = 2}, Block {val = 4}],
    [Block {val = 4}, Block {val = 2}, Block {val = 1}]]



initBoard :: BoardSize -> JewelSize -> Board -- TODO: generate board by bsize and gsize
initBoard bsize gsize = initBoardValue bsize gsize

shuffleBoard :: State -> State
shuffleBoard s = s -- TODO

drawState :: State -> [Widget ResourceName] -- TODO: also show score, etc.
drawState st = drawBoard (board st) (row st) (col st)

drawBoard :: Board -> Int -> Int -> [Widget ResourceName]
drawBoard bd row col = [vBox $ drawBoardHelper bd row col]

drawBoardHelper :: Board -> Int -> Int -> [Widget ResourceName]
drawBoardHelper bd row col = case bd of
                                [] -> []
                                x:xs -> if row == 0
                                            then (drawRow x col) : (drawBoard xs (-1) col)
                                            else (drawRow x (-1)) : (drawBoard xs (row - 1) col)

drawRow :: Row -> Int -> Widget n
drawRow r col = if col < 0
                    then hBox $ map drawBlock r
                    else hBox $ drawRowSelected r col

drawRowSelected :: Row -> Int -> [Widget n]
drawRowSelected r col = case r of
                            [] -> []
                            x:xs -> if col == 0
                                        then (drawBlockSelected x) : (drawRowSelected xs (-1))
                                        else (drawBlock x) : (drawRowSelected xs (col - 1))

drawBlock :: Block -> Widget n
drawBlock blk = str (show (val blk) ++ "  ") -- TODO

drawBlockSelected :: Block -> Widget n
drawBlockSelected blk = str (show (val blk) ++ "< ") -- TODO

initGame :: Difficulty -> IO State
initGame diff = let iBoard = initBoard 3 3 in
                let iState = State {board = iBoard, score = 0, selected = False, row = 0, col = 0} in
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
                EvKey (KChar 'a') [] -> continue $ (cancelBlocks s)
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 's') [] -> continue $ (shuffleBoard s)
                -- EvKey (KEnter) [] -> continue $ s {selected = True}
                -- EvKey (KUp) [] ->
                --     case selected s of
                --         True -> let newState = cancelBlocks (swapBlock s DirUp) in
                --                 if score newState > score s
                --                     then continue newState
                --                     else continue s
                --         False -> continue $ (moveCursor s DirUp)
                _ -> continue s
        _ -> continue s

-- moveCursor :: State -> Direction -> State
-- moveCursor s d = case d of
--                     DirUp -> let rowIndex = nonEmptyCursorSelection (board s) in
--                                 let colIndex = nonEmptyCursorSelection (nonEmptyCursorCurrent (board s)) in
--                                     let newBoard = nonEmptyCursorSelectIndex (rowIndex - 1) (board s) in
--                                         case newBoard of
--                                             Just newBoard' -> 

--                     _ -> s

-- swapBlock :: State -> Direction -> State
-- swapBlock s d = let b = board s in
--                 s

cancelBlocks :: State -> State
cancelBlocks s = 
    let oldBoard = board s in
        let removeBoard = (removeBlock oldBoard) in
            let newScore = (eliminatedNum removeBoard)
                newBoard = (addNewBlocks(downBlock(removeBoard))) in
                if (newScore == 0)
                    then s {board = newBoard, score = score (s)}
                    else s {board = newBoard, score = (score (s) + newScore)}
                    -- else (cancelBlocks s {board = newBoard, score = (score (s) + newScore)})


-- TODO: may rewrite by using mapWithIndex
removeBlock :: [[Block]] -> [[Block]]
removeBlock board = zipWith f [0..] board
          where 
            f  j row = zipWith3 f' (replicate (length row) j) [0..] row
            f' i j x = if ifBlockEliminable board (i) (j)
                        then Block {val = -1}
                        else x

eliminatedNum :: [[Block]] -> Int
eliminatedNum board = foldr f 0 board
    where f x acc =  (f' x) + acc
          f' :: [Block] -> Int
          f' = foldr (\x n -> if x == Block {val = -1} then n+1 else n) 0


addNewBlocks :: [[Block]] -> [[Block]]
addNewBlocks board = map f board
          where 
            f row = map f' row
            f' b = 
                if val(b) == -1
                    then Block{val=(1)}  -- TODO: add Random Block
                    else b


makeRandomInt::Int -> Int
makeRandomInt seed = do
  let gen = mkStdGen seed
  fst $ randomR (1, 10) gen

randomNum :: IO Int
randomNum = do
    num <- randomIO :: IO Int
    return (makeRandomInt num)

downBlock :: [[Block]] -> [[Block]]
downBlock block = transpose((transpose(transpose(map leftRow (transpose block)))))

mergeRow :: [Block] -> [Block]
mergeRow row = case row of
  x:xs -> if val(x) == -1 then mergeRow xs else x:(mergeRow xs)
  [] -> []

leftRow :: [Block] -> [Block]
leftRow row = (replicate ((length row) - (length x)) (Block {val = -1})) ++ x 
  where x = mergeRow row

leftBlock :: [[Block]] -> [[Block]]
leftBlock g = map leftRow g
       

ifBlockEliminable :: [[Block]] -> Int -> Int -> Bool
ifBlockEliminable board row column = matchInRow board row column
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
