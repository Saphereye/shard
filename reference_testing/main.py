import chess

board = chess.Board("k7/8/8/1q1Pp1K1/8/8/8/8 w - e6 0 2")
print(board)

truth = set([i.uci() for i in board.legal_moves])
test_string = "d5d6 d5e6 g5g4 g5h4 g5h5 g5f6 g5g6 g5h6"
test = set(test_string.split())
print(truth, '\n', test)

print("Size of truth: ", len(truth))
print("Size of test: ", len(test))

print("Extra moves: ", (test | truth) - (test & truth))
