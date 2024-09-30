# Shard Chess Engine
![](https://github.com/Saphereye/shard/blob/main/assets/demo.png)

# Roadmap
### Improve Evaluation Function
Enhance the evaluation function to consider more factors:
- [ ] King safety
- [ ] Pawn structure (isolated, doubled, passed pawns)
- [ ] Piece mobility
- [ ] Control of key squares (e.g., center control)
- [ ] Rook on open files
- [ ] Bishop pair advantage

### Enhance Move Ordering
Implement more advanced move ordering techniques:
- [ ] History heuristic: Track the success of moves in the search tree and prioritize moves that have historically caused beta cutoffs.
- [X] Killer move heuristic: Track moves that cause beta cutoffs at the same depth and prioritize them.
- [X] Principal variation search: Use the best move from the previous iteration as the first move in the current iteration.

### Improve Quiescence Search
Extend the quiescence search to include more types of moves, such as:
- [X] Checks
- [ ] Threats (moves that create immediate threats)

### Optimize Transposition Table
Use Zobrist hashing to efficiently hash board positions and implement a more sophisticated replacement scheme (e.g., always replace, depth- [ based replacement).

### Implement Advanced Search Techniques
- [X] **Iterative Deepening**: Perform a series of depth- [ limited searches, increasing the depth with each iteration. This helps with move ordering and time management.
- [ ] **Null Move Pruning**: Assume the opponent will pass their turn and see if the current player can still achieve a good position. If so, prune the branch.
- [ ] **Late Move Reductions**: Reduce the search depth for moves that are considered less likely to be good (e.g., moves that are not captures or checks).
- [ ] **Aspiration Windows**: Use a narrow window around the expected score to speed up alpha- [ beta search. If the score falls outside the window, re- [ search with a wider window.
