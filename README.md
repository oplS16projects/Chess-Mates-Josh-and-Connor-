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

Create several paragraphs of narrative to explain the pieces and how they interoperate.

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
