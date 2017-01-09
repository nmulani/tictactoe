# TicTacToe in Haskell

Using what I've learned in Haskell so far, I've coded a simple game of TicTacToe, where the user plays against the computer. To play, go to the source directory in your terminal, and then type "ghc tictactoe" or "stack ghc tictactoe" to compile.

Use "./tictactoe E" to run the game in "Easy" mode, where the computer will not actively block your moves, but will only try to arrange its own pieces in a winning combination. Use "./tictactoe H" to run the game in Hard mode, where the computer will try to block you from winning, and will also try to win with its own pieces:

```

> stack ghc tictactoe
> ./tictactoe E

```

Once you start the program, empty spaces on the board will be displayed with an 'E'. Your pieces will be 'X' and the computer's pieces will be 'O'. Follow the provided instructions and input a coordinate to place your piece on the board, in the following format: (row number, column number). Rows and columns start from 0, in the following fashion:

- (0,0) (0,1) (0,2)
- (1,0) (1,1) (1,2)
- (2,0) (2,1) (2,2)

Some possible opportunities for improvement:
- Color code tic-tac-toe board
- Give the user a hint about best next moves
- Add a GUI interface
- Allow two people to play each other
