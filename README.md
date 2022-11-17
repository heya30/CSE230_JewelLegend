# CSE230_JewelLegend

## Group Member
Ya He, Kehan Wang, Yumeng Sun, Bokai Zhang

## Proposal

### Introduction
In this project, we aim to develop a classic match 3 puzzle game called **Jewel Legend** using Haskell. For more rules of this game, please see Wiki for [Bejeweled](https://en.wikipedia.org/wiki/Bejeweled).

Expected game interface:

![Expected game interface](https://upload.wikimedia.org/wikipedia/en/0/05/Bejeweled_deluxe_sc1.jpg)


This game is to eliminate as many cubes as possible by swapping two adjacent cubes.
There are two situations when a player swaps two cubes:
- If the swap doesn’t cause elimination, this operation is invalid, and the board doesn’t change.
- If the swap causes more than three or more cubes of the same color to be connected in a straight line, these cubes are eliminated. After the upper cubes fill the vacant position, the remaining vacant position is randomly generated with a colored cube. Recheck for new eliminations after each elimination until no cubes can be eliminated.





### Goals
- Complete the basic game logic, including generating a map, eliminating cubes, swaping cubes, etc.
- Provide a user-friendly interface using Haskell brick library
- Allow users to set the different levels and the size of the board
- Allow users to swap cubes by controlling the keyboard
- Allow users to leave or restart during the game

### How to Play
Before setting up, remove `stack.yaml` and `stack.yaml.lock`, and try `stack init`

`stack setup`

`stack build`

`stack exec CSE230-JewelLegend`

### References
- [Brick](https://github.com/jtdaugherty/brick)

