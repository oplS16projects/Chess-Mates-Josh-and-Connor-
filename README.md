# Josh Blanchett & Conor Finegan

# ChessNuts: The People's Chess Simulator

### Statement

For this project, we are going to design and implement a simulation of the ancient board game Chess.  We both have some interest in the game of Chess.  We hope to learn how to incorporate both a stable, high-performing back end and a visually appealing UI. We hope to learn over the course of this project how to use the Racket language to solve a problem that would traditionally be solved using a more imperative language.

### Analysis
The game of Chess is inherently highly stateful, and thus we will not be using purely functional programming. We plan on using a mostly object-oriented design, with the basic moving parts of the game (piece, tile, board, etc.) represented by classes. We will be rolling our own object implementation using closures to capture member data. Polymorphism will be used to distinguish between different types of Chess pieces. We will almost certainly be using the high-order procedures `map` and `filter` to find the valid moves a given piece can make from its current position.

### Data set or other source materials
Were not (as of right now) going to be using any exterior data, besides using sprites for pieces.  Everything else is hard coded.

### Deliverable and Demonstration
In the end, we will have a functioning game of chess.  You will need two people to play this game because we are not planning on implementing an AI, but if time allows, we will look into networking.  It will be an interactive program that will not run off specific data.  Maybe we will ask what the players names are before the game starts.  This will be explored if time allows.

### Evaluation of Results
It will be a successful chess game if it forces the players to play by the rules of chess, and it does not crash.  Also, the game must be visually appealing.  If the user knows how to play chess, then they should not need any further instruction or explanation to use our program.

## Architecture Diagram
![alt-tag](https://github.com/oplS16projects/Chess-Mates-Josh-and-Connor-/blob/master/ArchDiagram.jpg)

The above diagram splits the program into three parts: Conor's responsibilites, Josh's responsibilities, and classes that will likely be worked on by both of us. Note that these diagrams represent interfaces to the rest of the program, and not necessarily entire implementations.

###Common Module:
The "common" section contains two classes: `board` and `tile`. 

The `tile` class represents an individual tile on the chess board. it is aware of its own X and Y coordinates (accessed through the `get-x` and `get-y` accessor methods) and the piece that it is holding (accessed through `get-piece`). If the tile is empty, then `get-piece` returns `'()`. We may implement an `is-empty?` method as a short-hand to check if the tile is empty. Additionally, `tile` contains a `set-piece` method which is used to move pieces around the board. Finally, the `draw` method will be used by Josh to draw an individual tile. This method will be responsible for drawing the tile at the correct location, as well as for calling the draw method for the piece it owns (if applicable).

The `board` class represents an entire chess board, and essentially represents a collection of tiles, which can be accessed by their X and Y coordinates. The `reset` method clears all tiles and puts the board in a "beginning of game" state. This method is called by the constructor, so hopefully a board will never be in an invalid state. The `get-tile-at` method returns a tile at a given X/Y location. The `get-all-tiles` method returns a list of all tiles, which will be useful for mapping/filtering over all tiles. We plan on storing the tiles in a flat list, which will (slightly) complicate the `get-tile-at` method implementation, but will make `get-all-tiles` very straightforward, and will hopefully help with optimization. Finally, the `draw` method will be used by Josh to draw the entire board at once. This method will be responsible for calling the draw methods for the individual tiles that it owns.

### Conor's Module:
Conor's module only contains one publicly accessible class, `piece`. This is an interface representing a chess piece. Chess pieces can be queried for their type using the `get-type` method, but beyond that look identical to an outside caller. The `get-team` method returns a symbol representing the piece's team, and the `draw` method will be used by Josh's code to get the sprite associated with the piece. The `get-valid-moves` method is where most of the complexity in this module will come from, as each chess piece will have significantly different criteria defining what a "valid" move is. This method will return a list of tiles, or an empty list if no valid moves exist. Finally, the `get-tile` method will return the tile that owns the calling piece. Although our diagram suggests that we will have an `attempt-move` method for this class, it will most likely be easiest for us to implement movement from the perspective of the board - the board would have a `move-piece` method that takes two tiles as arguments, and attempts to move any piece found on the first tile to the second tile.

### Josh's Module:
Josh's module represents the user interface and high-level game logic. His code is responsible for initialization of the game, detecting mouse clicks and key presses, and detecting when the game is over. Although his code is represented in the diagram as a class, it does not necessarily need to be implemented in an OOP fashion. In some respects, Josh is writing a program that will call into Conor's library. When the game starts, Josh's module will be responsible for instantiating a board class. As the game runs, his module will need to keep track of who's turn it is, and continuously check to make sure neither player has won. When one player's king is captured, his code will be responsible for displaying the winner and giving the user(s) an option to restart the game.

## Schedule
Explain how you will go from proposal to finished product. 

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Fri Apr 15)
**Conor:** 
Not all pieces working correctly, but having place holder functions implemented and full implementations for board and tile

**Josh:**
Having the board appear, and pieces on the board in the correct position.  Nothing would work, but the place holders are there and ready to go.

First Milestone update 4/15/16:  Josh has created a file called boardGUI.   If you open that file and write `(game)` in the repl, you will get a popup visual of a chess board.  

Conor has baseline placeholder functionality for moving pieces working. Currently you can only move a white pawn up one space at a time because this is the only piece with a valid `get-valid-moves` method, but the framework is there to move pieces around the board and check for the validity of these moves. You can paste the following series of expressions into the REPL fo for `board.rkt` to test this functionality.

The following code will successfully move a pawn from tile (0, 6) to (0, 5):
```
(define b (make-board))
((b 'draw))
(newline)

(define t ((b 'tile-at) 0 6))

((b 'move-piece) t ((b 'tile-at) 0 5))
(newline)

((b 'draw))
```

The following code will attempt to move a pawn from tile (0, 6) to (5, 5), but fails because this is not a valid move for a pawn:
```
(define b2 (make-board))
((b2 'draw))
(newline)

(define t2 ((b2 'tile-at) 0 6))

((b2 'move-piece) t2 ((b2 'tile-at) 5 5))
(newline)

((b2 'draw))
```

The following code will attempt to move a piece from tile (3, 3) to (4, 4), but fails because there is no piece on tile (3, 3):
```
(define b3 (make-board))
((b3 'draw))
(newline)

((b3 'move-piece) ((b3 'tile-at) 3 3) ((b3 'tile-at) 4 4))
```

### Second Milestone (Fri Apr 22)
**Conor:**
Basic, unoptimized path finding functions for pieces.

**Josh:**
Having Conor's code work with mine.  On-click and on-key working and functioning.  Maybe an on-mouse-over to know where the mouse is and act accordingly.

### Final Presentation (last week of semester)
Polishing off code.  Castling and detection of checks and checkmates.  Working on green colored tiles to indicate proper moves.  Making sure everything works together, and bug fixing.  In depth testing.  BUG FIXING!  BUG FIXING!
## Group Responsibilities

**Conor:**
Conor will be handling back end, rule enforcing code.  He is the enforcer.

**Josh:**
Josh will be handling the UI chess board, making everything look nice and appealing the users.  He is the tailor.
