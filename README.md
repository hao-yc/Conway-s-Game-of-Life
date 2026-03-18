# 🧬 Game of Life

An interactive implementation of **Conway's Game of Life** in Haskell, with real-time graphical visualization in the browser.

---

## Description

The Game of Life is a cellular automaton invented by John Conway in 1970. It consists of a grid of cells that can be alive or dead, evolving generation by generation according to four simple rules:

1. A live cell with fewer than 2 live neighbours **dies** (underpopulation).
2. A live cell with 2 or 3 live neighbours **survives**.
3. A live cell with more than 3 live neighbours **dies** (overpopulation).
4. A dead cell with exactly 3 live neighbours **becomes alive** (reproduction).

---

## Project Structure

```
life/
├── life.hs           # Main module: game state, actions, view
├── life.cabal        # Package configuration
└── Life/
    ├── Board.hs      # Board logic (Board type, generations, JSON)
    └── Draw.hs       # Drawing functions (cells and grid)
```

---

## Requirements

- [Stack](https://docs.haskellstack.org/) (Haskell project manager)
- **Chromium** browser (recommended — Firefox may have issues)

---

## Installation & Running

```bash
# Build
stack build

# Run
stack run life
```

Then open your browser at: [http://localhost:3708/](http://localhost:3708/)

---

## Controls

| Key | Action |
|---|---|
| `N` | Advance to the next generation |
| `G` | Cycle grid mode (None / Live cells / Full view) |
| `I` | Zoom in |
| `O` | Zoom out |
| `↑ ↓ ← →` | Pan the view |
| `H` | Show / hide help overlay |
| `S` | Save board to `board.json` |
| `L` | Load board from `board.json` |
| `Mouse click` | Toggle a cell alive / dead |

---

## Features

- **Real-time visualization** of the board in the browser via SVG.
- **Interactive editing**: click any cell to toggle its state.
- **Three grid modes**:
  - `NoGrid`: no grid displayed.
  - `LivesGrid`: grid surrounding the live cells only.
  - `ViewGrid`: grid covering the entire visible area.
- **Zoom and pan**: freely navigate the board.
- **Save and load** the board state in JSON format (`board.json`).
- **On-screen help overlay** toggled with `H`.

---

## Modules

### `Life.Board`

Defines the `Board` type and all game logic:

- `initBoard` — empty initial board.
- `setCell` — set a cell alive or dead.
- `nextGeneration` — compute the next generation.
- `cellIsLive`, `liveCells` — query cell states.
- `ToJSON` / `FromJSON` instances for serialization.

### `Life.Draw`

Drawing functions:

- `drawCell` — draws a live cell as a 1×1 solid rectangle.
- `drawBoard` — draws all live cells on the board.
- `drawGrid` — draws the grid between the given corner positions.

### `Main`

Contains the game state (`Game`), actions (`Action`), the `update` function and the `draw` function.

---

## Save Format

The board is saved as JSON in `board.json`:

```json
{
  "cells": [[0,0],[1,0],[2,0]],
  "minCol": 0,
  "maxCol": 2,
  "minRow": 0,
  "maxRow": 0
}
```

---

## Author Haoyan Chen

Practice 2 — Telematic Applications Design  
Telecommunications Engineering · Fall 2025
