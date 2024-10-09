# Shard Chess Engine

<img src="https://github.com/Saphereye/shard/blob/main/assets/demo.png" width="300" />

*Shard vs Shard (Shard Wins)*

## Overview

**Shard** is a chess engine built in Rust that supports most UCI (Universal Chess Interface) commands. The engine employs the **Monte Carlo Tree Search (MCTS)** algorithm to evaluate and make decisions. The goal of Shard is to explore the possibilities of integrating MCTS into chess, giving it a unique style of move generation and search depth.

## Key Features
- Written entirely in **Rust**.
- Supports **UCI** commands for easy integration with GUIs and other chess systems.
- Uses **Monte Carlo Tree Search (MCTS)** to evaluate moves based on exploration and exploitation trade-offs.
  
## How It Works

At the core of Shard's engine is a tree-like structure representing different game states (positions on the chessboard). Each "Node" corresponds to a board position, with additional details tracked for evaluation:

- **t:** The total score accumulated by visiting this position.
- **n:** The number of visits to this position (how many times the engine has evaluated this position in simulations).

After performing the simulations, to pick the _best move_, the engine picks the move with the most number of visits. The rationale is that the move shows the most promise.

### Example Tree Structure:
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

In this example, the top node represents the starting position of a chess game. Each subsequent node represents a move, with the total score (`t`) and visit count (`n`) helping the engine evaluate which positions are worth further exploration. The engine continuously updates this tree as moves are played, focusing more on moves that yield promising results based on prior simulations.

To choose the next node to explore, the evaluation is performed using the $\text{UCB1}$ score, which in this case is calculated as follows:

$$
\text{UCB1} = \frac{c_t}{c_n} + \sqrt{2} \times \sqrt{\frac{\log p_n}{c_n}}
$$

Here, $c_t$ and $c_n$ represent the total score and the number of visits to the current node. Similarly, $p_n$ represents the number of visits to the current node's parent. The constant $\sqrt{2}$ balances the _exploitation vs. the exploration_ term, corresponding to the left and right of the addition term on the RHS.

Here, $c_t$ and $c_n$ represent the total score and the number of visits to the current node, respectively, while $p_n$ represents the number of visits to the current node's parent. The constant $\sqrt{2}$ balances the exploitation (left term) and exploration (right term) components.

In traditional Monte Carlo Tree Search, the **rollout** phase consists of a simulation of the game that assigns a score of `+1/-1` for a win or loss. However, as the game reaches the endgame in chess, it becomes increasingly difficult for random moves to reach a conclusion. Therefore, I use a handcrafted board evaluation function to assign scores instead during rollouts.

## Getting Started

1. **Clone the repository:**
   ```bash
   git clone https://github.com/Saphereye/shard.git
   cd shard
   ```

2. **Run the engine:**
   ```bash
   chmod u+x run.sh
   ./run.sh
   ```

The `build.sh` script does the additional task of copying the executable to the lichess-bot project.

## License

This project is licensed under the GNU GPLv3 License. See the [LICENSE](LICENSE) file for details.

---

Feel free to contribute or open an issue if you have any feedback or suggestions!
