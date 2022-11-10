# CSE230_Minesweeper

## Group Member
Ya He, Kehan Wang, Yumeng Sun, Bokai Zhang

## Proposal

### Introduction
In this project, we aim to develop a classic logic puzzle game called Minesweeper using Haskell. For more rules of this game, please see Wiki for [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_(video_game)).

![](https://raw.githubusercontent.com/heya30/CSE230_Minesweeper/main/images/minesweeper.png)

The basic goal of the game is to open all non-mined cells in the shortest time.

There are three situations when the player choose to open a cell:
- If the cell contain a mine, the game ends and the player fails.
- If the cell does not contain a mine and there are no mines in the neighboring eight cells, the board will automatically open surrounding non-mined cells.
- If the cell does not contain a mine, but eight neighboring cells contain mines, the cell shows the number of mines in the neighboring cells.





### Goals
- Complete the basic game logic, including generating a map, opening cells, flagging a cell, etc.
- Provide a user-friendly interface using Haskell brick library
- Allow users to set the number of mines and the size of the board
- Allow users to select, open, flag cells and cancel flags by controlling the keyboard
- Allow users to leave or restart during the game



### Timeline

### References
- [Brick](https://github.com/jtdaugherty/brick)
