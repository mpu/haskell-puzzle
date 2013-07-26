# haskell-puzzle

That is a small haskell brute force program to solve
"Unblock Me" style puzzles with complex blocks.

## Usage

    $ ghc --make Puzzle.hs
    $ ./Puzzle < t/g.game | less -R

Simply pipe in the puzzle description. You should
see as a result a list of colored grids indicating
the steps to solve the puzzle. Some sample puzzle
descriptions are provided in the example
subdirectory.

## Syntax of game files

A game starts with the final coordinates of the
block 1. Then follows a line of 'x's specifying
the width of the grid. Finally the grid follows.
The grid contains digits and dots ('.'). Digits
mark blocks, dots empty space.

Here follows a sample grid.

    33.4444..
    33...2...
    ..2222211

## Screencast

A short screencast is available
[outside GitHub](http://c9x.me/~qcar/haskell-puzzle/).
