import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import pandas as pd
import numpy as np
import chess
from tqdm import tqdm
import os
import argparse

class ChessDataset(Dataset):
    def __init__(self, csv_file):
        self.data = pd.read_csv(csv_file, header=None, names=['fen', 'evaluation'])
        print(f"Loaded {len(self.data)} positions")
        
    def __len__(self):
        return len(self.data)
    
    def __getitem__(self, idx):
        row = self.data.iloc[idx]
        board = chess.Board(row['fen'])
        position = self.encode_position(board)
        evaluation = float(row['evaluation'])
        return torch.tensor(position, dtype=torch.float32), torch.tensor([evaluation], dtype=torch.float32)
    
    def encode_position(self, board):
        encoding = np.zeros(768, dtype=np.float32)  # 12 pieces * 64 squares
        piece_map = {
            (chess.PAWN, chess.WHITE): 0,
            (chess.KNIGHT, chess.WHITE): 1,
            (chess.BISHOP, chess.WHITE): 2,
            (chess.ROOK, chess.WHITE): 3,
            (chess.QUEEN, chess.WHITE): 4,
            (chess.KING, chess.WHITE): 5,
            (chess.PAWN, chess.BLACK): 6,
            (chess.KNIGHT, chess.BLACK): 7,
            (chess.BISHOP, chess.BLACK): 8,
            (chess.ROOK, chess.BLACK): 9,
            (chess.QUEEN, chess.BLACK): 10,
            (chess.KING, chess.BLACK): 11,
        }
        for square in chess.SQUARES:
            piece = board.piece_at(square)
            if piece:
                piece_idx = piece_map[(piece.piece_type, piece.color)]
                encoding[piece_idx * 64 + square] = 1.0
        return encoding

class NNUE(nn.Module):
    def __init__(self, hidden_size=512):
        super(NNUE, self).__init__()
        self.input_layer = nn.Linear(768, hidden_size)
        self.hidden_layer = nn.Linear(hidden_size, hidden_size) 
        self.output_layer = nn.Linear(hidden_size, 1)
        self.relu = nn.ReLU()
        
    def forward(self, x):
        x = self.relu(self.input_layer(x))
        x = self.relu(self.hidden_layer(x))
        x = self.output_layer(x)
        return x

def train_model(csv_file, epochs=50, batch_size=64, learning_rate=1e-4, save_path='best_nnue_model.pth', resume=False):
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")
    
    dataset = ChessDataset(csv_file)
    train_size = int(0.8 * len(dataset))
    val_size = len(dataset) - train_size
    train_dataset, val_dataset = torch.utils.data.random_split(dataset, [train_size, val_size])
    
    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True, num_workers=4)
    val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False, num_workers=4)
    
    model = NNUE(hidden_size=512).to(device)
    criterion = nn.MSELoss()
    optimizer = optim.Adam(model.parameters(), lr=learning_rate)
    scheduler = optim.lr_scheduler.StepLR(optimizer, step_size=20, gamma=0.5)
    
    start_epoch = 0
    best_val_loss = float('inf')

    if resume and os.path.exists(save_path):
        checkpoint = torch.load(save_path, map_location=device)
        model.load_state_dict(checkpoint)
        print(f"Resumed training from checkpoint: {save_path}")

    print(f"Model parameters: {sum(p.numel() for p in model.parameters()):,}")
    
    for epoch in range(start_epoch, epochs):
        model.train()
        train_loss = 0
        train_batches = 0
        
        for positions, evaluations in tqdm(train_loader, desc=f"Epoch {epoch+1}/{epochs}"):
            positions, evaluations = positions.to(device), evaluations.to(device)
            
            optimizer.zero_grad()
            outputs = model(positions)
            loss = criterion(outputs, evaluations)
            loss.backward()
            optimizer.step()
            
            train_loss += loss.item()
            train_batches += 1
        
        model.eval()
        val_loss = 0
        val_batches = 0
        
        with torch.no_grad():
            for positions, evaluations in val_loader:
                positions, evaluations = positions.to(device), evaluations.to(device)
                outputs = model(positions)
                loss = criterion(outputs, evaluations)
                val_loss += loss.item()
                val_batches += 1
        
        train_loss /= train_batches
        val_loss /= val_batches
        
        print(f"Epoch {epoch+1}: Train Loss: {train_loss:.4f}, Val Loss: {val_loss:.4f}")
        
        if val_loss < best_val_loss:
            best_val_loss = val_loss
            torch.save(model.state_dict(), save_path)
            print(f"New best model saved! Val Loss: {val_loss:.4f}")
        
        scheduler.step()
    
    return model

def export_for_rust(model_path='best_nnue_model.pth', export_dir='nnue_export'):
    model = NNUE(hidden_size=512)
    model.load_state_dict(torch.load(model_path, map_location='cpu'))
    model.eval()

    os.makedirs(export_dir, exist_ok=True)

    for name, param in model.named_parameters():
        arr = param.detach().numpy()
        filename = os.path.join(export_dir, f"{name}.npy")
        np.save(filename, arr)
        print(f"Saved {filename} shape={arr.shape}")

    with open(os.path.join(export_dir, 'model_config.txt'), 'w') as f:
        f.write("input_size=768\n")
        f.write("hidden_size=512\n")
        f.write("output_size=1\n")

    print("✅ Weights exported to individual .npy files in", export_dir)

def main():
    parser = argparse.ArgumentParser(description="Train or export a NNUE model for chess evaluation.")
    parser.add_argument('csv_file', type=str, help='CSV file with FEN positions and evaluations')
    parser.add_argument('--train', action='store_true', help='Train the model')
    parser.add_argument('--epochs', type=int, default=50, help='Number of training epochs')
    parser.add_argument('--batch_size', type=int, default=64, help='Batch size')
    parser.add_argument('--lr', type=float, default=1e-4, help='Learning rate')
    parser.add_argument('--resume', action='store_true', help='Resume training from checkpoint')
    parser.add_argument('--save_path', type=str, default='best_nnue_model.pth', help='Path to save or load model checkpoint')
    parser.add_argument('--export', action='store_true', help='Export model weights for Rust')
    parser.add_argument('--export_dir', type=str, default='nnue_export', help='Directory to export weights')
    
    args = parser.parse_args()

    if args.train:
        print("Starting training...")
        train_model(
            args.csv_file,
            epochs=args.epochs,
            batch_size=args.batch_size,
            learning_rate=args.lr,
            save_path=args.save_path,
            resume=args.resume
        )

    if args.export or (not args.train):
        print("Exporting model weights for Rust...")
        export_for_rust(model_path=args.save_path, export_dir=args.export_dir)

    print("Done!")

if __name__ == "__main__":
    main()
