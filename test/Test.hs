module Main
  ( main
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Game

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [test_move_cursor,test_swap,test_cancel,serializeTest]

state1 = State {
        board = [[Block {val = 4},Block {val = 2},Block {val = 2},Block {val = 1}],
                 [Block {val = 3},Block {val = 1},Block {val = -2},Block {val = 1}],
                 [Block {val = -3},Block {val = 1},Block {val = 2},Block {val = 2}],
                 [Block {val = 1},Block {val = 2},Block {val = 1},Block {val = 1}]],
        score=0,
        selected = False,
        jsize= 4,
        height = 4,
        width = 4,
        row = 0,
        col = 0,
        seed = 50,
        time = 0,
        step = 5,
        blockState = 0, 
        second_col = -1,
        second_row = -1,
        swap_valid = (True,0),
        shuffle_times = 3,
        gameState = 0
    }

-- move to (3,1)
state2 = state1{row = 3, col = 1}


-- press "enter"
state3 = state2{selected = True}

-- swap (direction = Left) valid
state4 = state3{board = [[Block {val = 4},Block {val = 2},Block {val = 2},Block {val = 1}],
                         [Block {val = 3},Block {val = 1},Block {val = -2},Block {val = 1}],
                         [Block {val = -3},Block {val = 1},Block {val = 2},Block {val = 2}],
                         [Block {val = 2},Block {val = 1},Block {val = 1},Block {val = 1}]],
                selected = False,  second_row = 3, second_col = 0, blockState = 4 }

-- cancel blocks
state5 = state4{board = [[Block {val = 4},Block {val = 2},Block {val = 2},Block {val = 1}],
                         [Block {val = 3},Block {val = 0},Block {val = -2},Block {val = 1}],
                         [Block {val = -3},Block {val = 0},Block {val = 2},Block {val = 2}],
                         [Block {val = 2},Block {val = 0},Block {val = 0},Block {val = 0}]],
                score = 5, blockState = 1}

-- down blocks
state6 = state5{board = [[Block {val = 4},Block {val = 0},Block {val = 0},Block {val = 0}],
                         [Block {val = 3},Block {val = 0},Block {val = 2},Block {val = 1}],
                         [Block {val = -3},Block {val = 0},Block {val = -2},Block {val = 1}],
                         [Block {val = 2},Block {val = 2},Block {val = 2},Block {val = 2}]]}
-- hBomb        
state_selected_hBomb = state1{row = 1, col = 2, selected = True}
state_swap_hBomb = state_selected_hBomb{board = [[Block {val = 4},Block {val = 2},Block {val = 2},Block {val = 1}],
                                                 [Block {val = -1},Block {val = -1},Block {val = -1},Block {val = -1}],
                                                 [Block {val = -3},Block {val = 1},Block {val = 2},Block {val = 2}],
                                                 [Block {val = 1},Block {val = 2},Block {val = 1},Block {val = 1}]],
                                        selected = False,  second_row = 1, second_col = 3, blockState = 4}


-- vBomb
state_selected_vBomb = state1{row = 2, col = 0, selected = True}
state_swap_vBomb = state_selected_vBomb{board = [[Block {val = -1},Block {val = 2},Block {val = 2},Block {val = 1}],
                                                     [Block {val = -1},Block {val = 1},Block {val = -2},Block {val = 1}],
                                                     [Block {val = -1},Block {val = 1},Block {val = 2},Block {val = 2}],
                                                     [Block {val = -1},Block {val = 2},Block {val = 1},Block {val = 1}]],
                                        selected = False,  second_row = 1, second_col = 0, blockState = 4}


state7 = State {
        board = [[Block {val = 3},Block {val = 3},Block {val = 2},Block {val = 1},Block {val = 4},Block {val = 2},Block {val = 1}],
                 [Block {val = 3},Block {val = 4},Block {val = -2},Block {val = 3},Block {val = 1},Block {val = 1},Block {val = 4}],
                 [Block {val = 1},Block {val = 2},Block {val = 2},Block {val = 1},Block {val = 2},Block {val = 4},Block {val = 1}],
                 [Block {val = 3},Block {val = 3},Block {val = 2},Block {val = 3},Block {val = 4},Block {val = 3},Block {val = 2}],
                 [Block {val = 1},Block {val = 2},Block {val = 3},Block {val = 4},Block {val = 1},Block {val = 3},Block {val = 4}],
                 [Block {val = 2},Block {val = 2},Block {val = 4},Block {val = 4},Block {val = 2},Block {val = 4},Block {val = 1}],
                 [Block {val = 2},Block {val = 4},Block {val = 2},Block {val = 3},Block {val = 4},Block {val = 3},Block {val = 1}]],
        score=0,
        selected = False,
        jsize= 4,
        height = 7,
        width = 7,
        row = 0,
        col = 0,
        seed = 50,
        time = 0,
        step = 5,
        blockState = 0, 
        second_col = -1,
        second_row = -1,
        swap_valid = (True,0),
        shuffle_times = 3,
        gameState = 0
    }


state8 = State {
        board =[[Block {val = 2},Block {val = 4},Block {val = 3},Block {val = 4},Block {val = 2},Block {val = 2},Block {val = 4}],
                 [Block {val = 4},Block {val = 4},Block {val = 1},Block {val = 4},Block {val = 1},Block {val = 3},Block {val = 1}],
                 [Block {val = -3},Block {val = 2},Block {val = 4},Block {val = 3},Block {val = 3},Block {val = 1},Block {val = 2}],
                 [Block {val = 4},Block {val = 4},Block {val = 3},Block {val = 2},Block {val = 4},Block {val = 1},Block {val = 3}],
                 [Block {val = 1},Block {val = 3},Block {val = 1},Block {val = -3},Block {val = 1},Block {val = 3},Block {val = 4}],
                 [Block {val = -2},Block {val = 2},Block {val = 3},Block {val = 1},Block {val = 2},Block {val = 4},Block {val = 3}],
                 [Block {val = -3},Block {val = 4},Block {val = 3},Block {val = 1},Block {val = 2},Block {val = 1},Block {val = 4}]],
        score=0,
        selected = False,
        jsize= 4,
        height = 4,
        width = 4,
        row = 0,
        col = 0,
        seed = 50,
        time = 0,
        step = 5,
        blockState = 0, 
        second_col = -1,
        second_row = -1,
        swap_valid = (True,0),
        shuffle_times = 3,
        gameState = 0
    }



-- test1 =  testCase "Cancel Blocks1" $
--   (cancelBlocks state1) @?= state1

test_move_cursor :: TestTree
test_move_cursor = testGroup "Move Cursor" [test2,test2',test3,test3',test4,test4',test5,test5']

test2 =  testCase "Move cursor down (Valid)" $
         handleDirection state1 DirDown @?= state1{row = 1}

test2' =  testCase "Move cursor down (Invalid)" $
         handleDirection state2 DirDown @?= state2

test3 =  testCase "Move cursor up (Valid)" $
         handleDirection state2 DirUp @?= state2{row = 2}

test3' =  testCase "Move cursor up (Invalid)" $
         handleDirection state1 DirUp @?= state1

test4 =  testCase "Move cursor right (Valid)" $
         handleDirection state1 DirRight @?= state1{col = 1}

test4' =  testCase "Move cursor right (Invalid)" $        
         handleDirection state2{col = 3} DirRight @?= state2 {col = 3}

test5 =  testCase "Move cursor left (Valid)" $
         handleDirection state2 DirLeft @?= state2{col = 0} 

test5' =  testCase "Move cursor left (Invalid)" $
         handleDirection state1 DirLeft @?= state1
        --  handleDirection state2 DirLeft @?= state2{col=4}


test_swap :: TestTree
test_swap = testGroup "Swap Two Blocks" [test6,test7,test8,test9]

test6 = testCase "Valid Swap" $
        handleDirection state3 DirLeft @?= state4
        -- swapByDir state3 DirLeft @?= state4

test7 = testCase "InValid Swap" $
        handleDirection state1{selected = True} DirRight @?= state1{swap_valid = (False,3)}


test8 = testCase "Swap hBomb" $
        handleDirection state_selected_hBomb DirRight @?= state_swap_hBomb


test9 = testCase "Swap vBomb" $
        handleDirection state_selected_vBomb DirUp @?= state_swap_vBomb


test_cancel :: TestTree
test_cancel = testGroup "After swap" [test10,test11,test12]

test10 =  testCase "Cancel Blocks" $
        (cancelBlocks state4){seed = 0} @?= state5{seed = 0}


test11 =  testCase "Blocks Down" $
        (downBlock $ board state5) @?= board state6


test12 =  testCase "Add New Blocks" $
        check_full state6 (addNewBlocks (board state6) (jsize state6) (seed state6)) @?= True


-- check the board is correct after adding new blocks 
check_full :: State -> Board ->Bool
check_full s b' = let b = board s in
                        [[let num' = val ((b' !! r) !! c) 
                              num = val x in
                                if (num == -1)||(num == 0) 
                                then num' <= (jsize s) && (num' > 0)  -- if block is empty, assign a jewelVal
                                else num == num' -- if block not empty, jewelVal is not change
                                | (c, x) <- zip [0..length temp - 1] temp] | (r, temp) <- zip [0..length b - 1] b] 
                        ==[[True | x <- row ] | row <- b]


handleTickEvent_continuously ::State -> State
handleTickEvent_continuously s = if (blockState s) /= 0
                                        then handleTickEvent_continuously $ handleTickEvent s
                                        else  s
selectBlock :: State -> State
selectBlock s = s{selected = True}

serializeTest :: TestTree
serializeTest = testGroup "Serialized Test" [test13,test14,test15]


test13 = testCase "Test-1" $ 
                       let s = (handleTickEvent_continuously $ handleDirection state3 DirUp )
                           flag = (score s >=score  state3) && (time s < time state3) in
                        (jsize s, row s, col s, selected s, flag) @?= (jsize state3, row state3, col state3, False, True)


test14 = testCase "Test-2" $ 
                       let s = (handleTickEvent_continuously $ handleDirection (selectBlock $ handleDirection (handleDirection (handleDirection state7 DirDown) DirRight) DirRight) DirUp)
                           flag = (score s >=score  state7) && (time s < time state7) in
                        (jsize s, selected s, flag) @?= (jsize state7, False, True)


test15 = testCase "Test-3" $ 
                       let s = (handleTickEvent_continuously $ handleDirection (selectBlock $ handleDirection (handleDirection state8 DirDown) DirDown)  DirUp)
                           flag = (score s >=score  state7) && (time s < time state7) in
                        (jsize s, selected s, flag) @?= (jsize state7, False, True)