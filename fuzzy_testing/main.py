import chess
import subprocess
import random

def generate_random_fen():
    """Generates a random legal board position (FEN string)."""
    board = chess.Board()
    move_count = random.randint(0, 100)  # Random number of moves to play

    for _ in range(move_count):
        if board.is_game_over():
            break
        legal_moves = list(board.legal_moves)
        board.push(random.choice(legal_moves))  # Play random legal move

    return board.fen()

def get_legal_moves_from_executable(fen):
    """Runs the executable with the given FEN string and gets legal moves."""
    # Modify this command to point to your executable's path
    result = subprocess.run(['/home/adarsh/Coding/shard/target/release/shard', fen], capture_output=True, text=True)

    # Assuming the executable outputs moves as space-separated UCI strings
    moves = result.stdout.strip().split()
    return moves

def run_fuzzy_test():
    """Performs fuzzy testing by comparing executable output with python-chess."""
    i = 0
    subprocess.run(["cargo", "build", "--release"])
    while True:
        print(f"Test iteration {i + 1}")

        # Generate random FEN
        random_fen = generate_random_fen()
        print("Testing FEN:", random_fen)

        # Get legal moves using python-chess
        board = chess.Board(random_fen)
        python_moves = [move.uci() for move in board.legal_moves]

        # Get legal moves from executable
        executable_moves = get_legal_moves_from_executable(random_fen)

        # Compare results
        python_moves_set = set(python_moves)
        executable_moves_set = set(executable_moves)

        if python_moves_set != executable_moves_set:
            print(f"Discrepancy found in test {i + 1}!")
            print(f"FEN: {random_fen}")
            print(f"python-chess moves: {sorted(python_moves_set)}")
            print(f"Executable moves: {sorted(executable_moves_set)}")
            print(f"Moves in python-chess not present in executable: {sorted(python_moves_set - executable_moves_set)}")
            print(f"Moves in executable not present in python-chess: {sorted(executable_moves_set - python_moves_set)}")
            break
        else:
            print(f"Test {i + 1} passed!\n")
        i += 1

if __name__ == "__main__":
    run_fuzzy_test()
