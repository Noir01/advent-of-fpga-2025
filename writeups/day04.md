# SPOILERS!!!
This writeup assumes that you've already read AND solved day 4. If you haven't, you'll probably get spoiled.

### The Problem

We have a grid of cells (`.` = empty, `@` = alive) and need to simulate a cellular automaton. A cell is "weak" if it's alive but has fewer than 4 neighbors (out of 8 possible). Weak cells die in the next generation.

We need to compute:
- **Part 1**: Count of weak cells in the initial grid
- **Part 2**: Total count of weak cells that die across ALL generations until the grid stabilizes

The twist for Part 2 is that updates happen "in-place" - when processing a cell, we use the already-updated values for neighbors above and to the left (since they were processed earlier in the same generation).

### Input Encoding

The parser (`day04.ml`) reads the grid and adds a 1-cell border of empty cells around all sides. This greatly simplifies boundary handling since we never need to check for edge cases. Each row is encoded as a bit vector where `0` = empty, `1` = alive.

```ocaml
let encode_line line =
  "0" (* left border *)
  ^ String.concat_map line ~f:(fun c ->
    match c with
    | '.' -> "0"
    | '@' -> "1"
    | _ -> failwith ...)
  ^ "0" (* right border *)
```

The grid dimensions are packed into a 16-bit word: high 8 bits = width, low 8 bits = height.

### Solver State Machine

The solver (`day04_solver.ml`) implements a 6-state FSM with double-buffered BRAM storage:

- **Idle**: Initialize registers, unpack grid dimensions from `input_count`, wait for `start`
- **Load_row**: Copy rows from external RAM into BRAM A (one row per cycle)
- **Prime**: Load 3 consecutive rows into shift registers (handles BRAM's 1-cycle read latency)
- **Scan**: Process cells column by column, computing neighbor counts and updating the output row
- **Write_row**: Write the processed row to the output BRAM buffer, shift rows down, load next row
- **Done**: Signal completion, outputs are valid

### The Sliding Window

The key optimization is the 3-row sliding buffer. Instead of random-accessing the entire grid, we maintain three row registers:

```ocaml
let%hw_var row_top = Variable.reg spec ~width:data_width in
let%hw_var row_mid = Variable.reg spec ~width:data_width in
let%hw_var row_bot = Variable.reg spec ~width:data_width in
let%hw_var output_row = Variable.reg spec ~width:data_width in
```

For each cell in `row_mid`, we can access all 8 neighbors: 3 from `row_top`, 2 from `row_mid` (left/right), and 3 from `row_bot`.

### Neighbor Counting

The solver extracts individual bits using a mux over bit positions:

```ocaml
let get_bit row col =
  let bit_idx = grid_width.value -:. 1 -: col in
  mux bit_idx (bits_lsb row)
```

For Part 1, all 8 neighbors come from the original row buffers:

```ocaml
let get_neighbors_original () =
  [ get_bit row_top.value col_left   (* NW *)
  ; get_bit row_top.value col        (* N *)
  ; get_bit row_top.value col_right  (* NE *)
  ; get_bit row_mid.value col_left   (* W *)
  ; get_bit row_mid.value col_right  (* E *)
  ; get_bit row_bot.value col_left   (* SW *)
  ; get_bit row_bot.value col        (* S *)
  ; get_bit row_bot.value col_right  (* SE *)
  ]
```

For Part 2's in-place semantics, neighbors above and to the left use already-processed values:

```ocaml
let get_neighbors_inplace () =
  [ get_bit row_top.value col_left   (* NW - processed *)
  ; get_bit row_top.value col        (* N - processed *)
  ; get_bit row_top.value col_right  (* NE - processed *)
  ; get_bit output_row.value col_left (* W - use output_row *)
  ; get_bit row_mid.value col_right  (* E - original *)
  ; get_bit row_bot.value col_left   (* SW - original *)
  ; get_bit row_bot.value col        (* S - original *)
  ; get_bit row_bot.value col_right  (* SE - original *)
  ]
```

### The Cell Update Logic

A cell survives if it has 4 or more neighbors:

```ocaml
let neighbor_count = ... (* sum of neighbor bits *)
let is_weak = center &: (neighbor_count <:. 4)
let survives = center &: (neighbor_count >=:. 4)
```

The new cell value depends on which part we're solving:

```ocaml
let new_cell = mux2 part1_mode.value center survives
```

Part 1 keeps the original value (we just count weak cells), while Part 2 actually kills weak cells.

### Double Buffering

For Part 2, we need to run multiple generations until the grid stabilizes. The solver uses two BRAM banks (A and B) and alternates between them:

```ocaml
let%hw_var active_buffer = Variable.reg spec ~width:1 in
(* 0 = read A/write B, 1 = read B/write A *)
```

After each generation, if any cells died (`changes_this_gen`), we swap buffers and run another pass:

```ocaml
if_ changes_this_gen.value
  [ active_buffer <-- ~:(active_buffer.value)
  ; (* reset for next generation *)
    ...
    sm.set_next Prime
  ]
  [ sm.set_next Done ]
```

### Registers

The main registers used:
- `grid_width` / `grid_height` (8-bit each): Grid dimensions including border
- `row_idx` / `col_idx` (8-bit): Current position in the grid
- `row_top` / `row_mid` / `row_bot` (137-bit): The 3-row sliding window
- `output_row` (137-bit): Accumulates the processed row before writing to BRAM
- `part1_mode` (1-bit): Selects between Part 1 (counting only) and Part 2 (simulation)
- `changes_this_gen` (1-bit): Tracks if any cells died this generation
- `active_buffer` (1-bit): Which BRAM buffer to read from
- `part1_acc` / `part2_acc` (64-bit): Final result accumulators
