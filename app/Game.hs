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
import Data.Time.Clock

import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty

import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)

data Tick = Tick

-- positive integers for jewels
-- 0 for eliminated jewel
-- -1 for bombed jewel
-- -2 for horizontal bomb block
-- -3 for vertical bomb block
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
        jsize::Int,
        height::Int,
        width::Int,
        row::Int,
        col::Int,
        seed::Int,
        time::Float,
        step::Int,
        blockState::Int

    }
    deriving (Eq, Show)

type ResourceName = ()

blueBg, cyanBg, magBg, yellowBg, greenBg, brightBlueBg, brightCyanBg, brightMagBg, brightGreenBg, redBg, whiteFg, titleFg, redFg :: AttrName

blueBg = attrName "blueBg"
cyanBg = attrName "cyanBg"
magBg = attrName "magBg"
yellowBg = attrName "yellowBg"
greenBg = attrName "greenBg"

brightBlueBg = attrName "brightBlueBg"
brightCyanBg = attrName "brightCyanBg"
brightMagBg = attrName "brightMagBg"
brightGreenBg = attrName "brightGreenBg"

redBg = attrName "redBg"

whiteFg = attrName "whiteFg"
titleFg = attrName "titleFg"
scoreFg = attrName "scoreFg"
redFg = attrName "redFg"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (blueBg, U.bg V.blue),
  (cyanBg, U.bg V.cyan),
  (magBg, U.bg V.magenta),
  (yellowBg, U.bg V.yellow),
  (greenBg, U.bg V.green),
  (brightBlueBg, U.bg V.brightBlue),
  (brightCyanBg, U.bg V.brightCyan),
  (brightMagBg, U.bg V.brightMagenta),
  (brightGreenBg, U.bg V.brightGreen),
  (redBg, U.bg V.red),
  (whiteFg, U.fg V.white),
  (titleFg, U.fg V.cyan `V.withStyle` V.bold),
  (scoreFg, U.fg V.white `V.withStyle` V.bold),
  (redFg, U.fg V.red)
  ]


initBoard :: Int -> Int -> Board
initBoard height width = replicate height (replicate width Block {val = 0})

shuffleBoard :: State -> State
shuffleBoard s = let newState = initFirstBoard (s {board = initBoard (height s) (width s), score = 0, selected = False}) in
                    newState {score = score s, seed = (seed s + seed newState)}



drawState :: State -> [Widget ResourceName] -- TODO: also show score, etc.
drawState st = [C.center $  padRight (Pad 4) (drawBoard st) <+> 
    ((drawScore (round (time st)) (" time ")) <=> (drawScore (score st) " Score ") <=> padTop (Pad 2) drawHelp)
    ]

drawScore :: Int -> String -> Widget ResourceName
drawScore score title = hLimit 20
  $ withBorderStyle BS.unicodeBold
  $ withAttr titleFg
  $ B.borderWithLabel (str title)
  $ withAttr scoreFg
  $ C.hCenter
  $ padAll 1
  $ str $ show score

drawHelp :: Widget ResourceName
drawHelp = hLimit 20
  $ withBorderStyle BS.unicodeBold
  $ withAttr titleFg
  $ B.borderWithLabel (str " Commands ")
  $ withAttr whiteFg
  $ vBox $ map (uncurry drawCommand)
  $ [ ("Up", "↑")
    , ("Left", "←")
    , ("Right", "→")
    , ("Down", "↓")
    , ("Select", "Enter")
    , ("Shuffle", "s")
    , ("Quit", "Esc")
    ]
  where
    drawCommand s1 s2 = (padRight Max $ padLeft (Pad 1) $ str s1) <+> (padLeft Max $ padRight (Pad 1) $ str s2)

lBlock,sBlock :: String
lBlock = "    \n    "
sBlock = "  "

strBlock :: JewelVal -> String
strBlock v = "     \n  " ++ show v ++ "  \n     "

hBombBlock :: String
hBombBlock = "     \n --- \n     "

vBombBlock :: String
vBombBlock = "     \n  |  \n     "

color :: JewelVal -> Widget ()
color val = case val of
  -2-> withAttr redBg $ str (hBombBlock)
  -3-> withAttr redBg $ str (vBombBlock)
  1 -> withAttr blueBg $ str (strBlock val)
  2 -> withAttr cyanBg $ str (strBlock val)
  3 -> withAttr magBg $ str (strBlock val)
  4 -> withAttr yellowBg $ str (strBlock val)
  5 -> withAttr greenBg $ str (strBlock val)
  6 -> withAttr brightBlueBg $ str (strBlock val)
  7 -> withAttr brightCyanBg $ str (strBlock val)
  8 -> withAttr brightMagBg $ str (strBlock val)
  9 -> withAttr brightGreenBg $ str (strBlock val)
  _ -> str (strBlock val)


drawBoard :: State -> Widget ResourceName
drawBoard st = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (withAttr titleFg $ str " Jewel Legend ")
  $ vBox rows
  where
    bd = board st
    col' = col st
    row' = row st
    rows = [hBox $  blocksInRow i rowvals |(i, rowvals) <- zip [0..height st - 1] bd]
    blocksInRow i rowvals = [hLimit 8 $ vLimit 5 $ drawBlock i j block row' col' (selected st) | (j, block) <- zip [0..width st - 1] rowvals]

drawBlock :: Int -> Int -> Block -> Int -> Int -> Bool -> Widget ResourceName
drawBlock i j block row' col' s
        | (i == row')&& (j == col') && (s) = C.center $ B.border $ color $ val block -- TODO: signal selected block
        | (i == row')&& (j == col') = C.center $ B.border $ color $ val block
        | otherwise =  C.center $  color $ val block


getSeed :: IO Int
getSeed = do t <- getCurrentTime
             return $
                let n = read (show (diffTimeToPicoseconds (utctDayTime t))) in 
                    mod (div n 1000) 1000000

initGame :: Difficulty -> IO () -- TODO
initGame diff = let height = 5
                    width = 6
                    jsize = 4
                    step = 5 in
                let iBoard = initBoard height width in
                do
                    iSeed <- getSeed
                    chan <- newBChan 10
                    forkIO $ forever $ do
                        writeBChan chan Tick
                        threadDelay 200000 -- decides how fast your game moves
                    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
                    initialVty <- buildVty
                    void $ customMain initialVty buildVty (Just chan) jLApp (initFirstBoard State {
                                                            board = iBoard,
                                                            score = 0,
                                                            selected = False,
                                                            jsize = jsize,
                                                            height = height,
                                                            width = width,
                                                            row = 0,
                                                            col = 0,
                                                            step = step,
                                                            seed = iSeed,
                                                            time = 60,
                                                            blockState = 0
                                                            }) {score = 0}
                    return ()

jLApp :: App State Tick ResourceName
jLApp = App 
        {
            appDraw = drawState,
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleSelectEvent,
            appStartEvent = pure,
            appAttrMap = const theMap
        }

handleSelectEvent :: State -> BrickEvent n Tick -> EventM n (Next State)
handleSelectEvent s e =
    if step s == 0 then halt s
    else case e of 
        AppEvent Tick -> continue $ handleTickEvent s
        VtyEvent vtye -> 
            case vtye of
                EvKey (KChar 'a') [] -> continue $ (cancelBlocks s)
                EvKey (KEsc) [] -> halt s
                EvKey (KChar 's') [] -> continue $ (shuffleBoard s)
                EvKey (KEnter) [] -> continue $ s {selected = True}
                EvKey (KUp) [] -> continue $ (handleDirection s DirUp)
                EvKey (KDown) [] -> continue $ (handleDirection s DirDown)
                EvKey (KLeft) [] -> continue $ (handleDirection s DirLeft)
                EvKey (KRight) [] -> continue $ (handleDirection s DirRight)
                _ -> continue s
        _ -> continue s

handleTickEvent :: State -> State
handleTickEvent s =  if (blockState s) == 1
                        then s {time = (time s) - 0.2, board = downBlock (board s), blockState = 2}
                        else if (blockState s) == 2
                            then s {time = (time s) - 0.2, board = addNewBlocks (board s) (jsize s) (seed(s)), blockState = 3}
                            else (cancelBlocks s) {time = (time s) - 0.2}

handleDirection :: State -> Game.Direction -> State
handleDirection s d = case selected s of
                        True -> let newState = cancelBlocks (swapByDir s d) in
                                if score newState > score s
                                    then newState {selected = False, step = (step (s) - 1)}
                                    else s {selected = False}
                        False -> moveCursor s d

moveCursor :: State -> Game.Direction -> State
moveCursor s d = case d of
                    DirUp -> if row s > 0
                                then s {row = (row s) - 1}
                                else s
                    DirDown -> if row s < length (board s) - 1
                                then s {row = (row s) + 1}
                                else s
                    DirLeft -> if col s > 0
                                then s {col = (col s) - 1}
                                else s
                    DirRight -> if col s < length (head (board s)) - 1
                                then s {col = (col s) + 1}
                                else s

swapByDir :: State -> Game.Direction -> State
swapByDir s d = let b = board s
                    r = row s
                    c = col s
                    newR = row (moveCursor s d)
                    newC = col (moveCursor s d) in
                    if (val ((b!!r)!!c) == -2) && ((d == DirLeft) || (d == DirRight)) && (newC /= c)
                        then s {board = hBomb b r, selected = False} -- horizontal bomb
                        else if (val ((b!!r)!!c) == -3) && ((d == DirUp) || (d == DirDown)) && (newR /= r)
                            then s {board = vBomb b c, selected = False} -- vertical Bomb
                            else s {board = swapBlock b r c newR newC, selected = False}

hBomb :: Board -> Int -> Board
hBomb b row = [[get r c x | (c, x) <- zip [0..length temp - 1] temp] | (r, temp) <- zip [0..length b - 1] b]
                    where get r c x | (r == row) = Block {val = -1}
                                    | otherwise = x

hBombGen :: Board -> Int -> Int -> Bool
hBombGen b row col = let curVal = val ((b!!row)!!col) in
                        (curVal > 0)
                        && (col + 3 < length (b!!row))
                        &&
                        (
                            (
                            (col == 0) 
                            && (val ((b!!row)!!(col+1)) == curVal) 
                            && (val ((b!!row)!!(col+2)) == curVal) 
                            && (val ((b!!row)!!(col+3)) == curVal)
                            )
                        ||  (
                            (col > 0)
                            && (val ((b!!row)!!(col-1)) /= curVal)
                            && (val ((b!!row)!!(col+1)) == curVal) 
                            && (val ((b!!row)!!(col+2)) == curVal) 
                            && (val ((b!!row)!!(col+3)) == curVal)
                            )
                        )

vBomb :: Board -> Int -> Board
vBomb b col = [[get r c x | (c, x) <- zip [0..length temp - 1] temp] | (r, temp) <- zip [0..length b - 1] b]
                    where get r c x | (c == col) = Block {val = -1}
                                    | otherwise = x

vBombGen :: Board -> Int -> Int -> Bool
vBombGen b row col = let curVal = val ((b!!row)!!col) in
                        (curVal > 0)
                        && (row - 3 >= 0)
                        &&
                        (
                            (
                            (row == length b - 1) 
                            && (val ((b!!(row-1))!!col) == curVal) 
                            && (val ((b!!(row-2))!!col) == curVal) 
                            && (val ((b!!(row-3))!!col) == curVal)
                            )
                        ||  (
                            (row < length b - 1) 
                            && (val ((b!!(row+1))!!col) /= curVal)
                            && (val ((b!!(row-1))!!col) == curVal) 
                            && (val ((b!!(row-2))!!col) == curVal) 
                            && (val ((b!!(row-3))!!col) == curVal)
                            )
                        )

swapBlock :: Board -> Int -> Int -> Int -> Int -> Board
swapBlock b row1 col1 row2 col2 = [[get r c x | (c, x) <- zip [0..length temp - 1] temp] | (r, temp) <- zip [0..length b - 1] b]
                                    where get r c x | (r == row1) && (c == col1) = (b !! row2) !! col2
                                                    | (r == row2) && (c == col2) = (b !! row1) !! col1
                                                    | otherwise = x

initFirstBoard :: State -> State
initFirstBoard s = 
    let oldBoard = board s in
        let removeBoard = (removeBlock oldBoard) in
            let newScore = (eliminatedNum removeBoard) 
                newBoard = (addNewBlocks(downBlock(removeBoard)) (jsize s) (seed(s))) in
                if (newScore == 0)
                    then s {selected = False}
                    else (initFirstBoard s {board = newBoard, score = (score (s) + newScore), 
                                            selected = False, seed = ((score (s) + newScore) * 991 + seed(s))})


cancelBlocks :: State -> State
cancelBlocks s = 
    let oldBoard = board s in
        let removeBoard = (removeBlock oldBoard) in
            let newScore = (eliminatedNum removeBoard) in
                -- newBoard = (addNewBlocks(downBlock(removeBoard)) (jsize s) (seed(s))) in
                if (newScore == 0)
                    then s 
                    else (s {board = removeBoard, score = (score (s) + newScore), 
                                seed = ((score (s) + newScore) * 991 + seed(s)), blockState = 1})


-- TODO: may rewrite by using mapWithIndex
removeBlock :: [[Block]] -> [[Block]]
removeBlock board = zipWith f [0..] board
          where 
            f  j row = zipWith3 f' (replicate (length row) j) [0..] row
            f' i j x =  if hBombGen board i j then Block {val = -2}
                            else if vBombGen board i j then Block {val = -3}
                                    else if ifBlockEliminable board (i) (j) then Block {val = 0}
                                            else x

eliminatedNum :: [[Block]] -> Int
eliminatedNum board = foldr f 0 board
    where f x acc =  (f' x) + acc
          f' :: [Block] -> Int
          f' = foldr (\x n -> if (val x == 0) || (val x == -1) then n+1 else n) 0


addNewBlocks :: Board -> Int -> Int -> Board
addNewBlocks board jsize seed = zipWith f [0..] board
          where 
            f  j row = zipWith3 f' (replicate (length row) j) [0..] row
            f' i j x = if ((val x == 0) || (val x == -1))
                        then Block {val = (makeRandomInt jsize (seed + 67*i + 3*j))}
                        else x

makeRandomInt::Int -> Int -> Int
makeRandomInt jsize seed = do
  let gen = mkStdGen seed
  fst $ randomR (1, jsize) gen

downBlock :: [[Block]] -> [[Block]]
downBlock block = transpose((transpose(transpose(map leftRow (transpose block)))))

mergeRow :: [Block] -> [Block]
mergeRow row = case row of
  x:xs -> if ((val x == 0) || (val x == -1)) then mergeRow xs else x:(mergeRow xs)
  [] -> []

leftRow :: [Block] -> [Block]
leftRow row = (replicate ((length row) - (length x)) (Block {val = 0})) ++ x 
  where x = mergeRow row

leftBlock :: [[Block]] -> [[Block]]
leftBlock g = map leftRow g
       

ifBlockEliminable :: [[Block]] -> Int -> Int -> Bool
ifBlockEliminable board row column = (val ((board!!row)!!column) > 0) && 
                                        (matchInRow board row column || matchInRow (transpose board) column row)


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
