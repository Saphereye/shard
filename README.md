# Shard Chess Engine
<img src="https://github.com/Saphereye/shard/blob/main/assets/demo.png" width="500" />

*Shard Vs Shard (Shard Wins)*

## Introduction
Shard is a chess engine written in Rust. It is supports most UCI commands.
The engine utilizes the Monte Carlo Tree Search algorithm.

The tree looks like this:
```
Node t:16396, n:36 (rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1) (None)
    Node t:11048, n:8 (rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq - 0 1) (g2g4)
        Node t:538, n:1 (rnbqkbnr/1ppppppp/8/p7/6P1/8/PPPPPP1P/RNBQKBNR w KQkq - 0 1) (a7a5)
        Node t:-477, n:1 (rnbqkbnr/1ppppppp/p7/8/6P1/8/PPPPPP1P/RNBQKBNR w KQkq - 0 1) (a7a6)
        Node t:-553, n:1 (rnbqkbnr/p1pppppp/8/1p6/6P1/8/PPPPPP1P/RNBQKBNR w KQkq - 0 1) (b7b5)
        ...
    Node t:7211, n:6 (rnbqkbnr/pppppppp/8/8/8/7P/PPPPPPP1/RNBQKBNR b KQkq - 0 1) (h2h3)
        Node t:-137, n:1 (rnbqkbnr/1ppppppp/8/p7/8/7P/PPPPPPP1/RNBQKBNR w KQkq - 0 1) (a7a5)
        Node t:-1526, n:1 (rnbqkbnr/1ppppppp/p7/8/8/7P/PPPPPPP1/RNBQKBNR w KQkq - 0 1) (a7a6)
        ...
    ...
```
Each 'Node' represents a position in the game. The 't' value is the total score of the position, and the 'n' value is the number of times the position has been visited.
When a move is made, the engine will set the current position to the position of the move, and the tree will be updated accordingly from there.
