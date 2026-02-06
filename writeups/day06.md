# SPOILERS!!!
This writeup assumes that you've already read AND solved day 6. If you haven't, you'll probably get spoiled.

### The Problem

We're given a grid of numbers arranged in blocks, where each block has an operator (`+` or `*`) in the last row. We need to:

- **Part 1**: Read numbers horizontally (row by row within each block), then apply the block's operator to combine them
- **Part 2**: Read numbers vertically (column by column within each block), then apply the block's operator to combine them

The input looks something like this:
```
91   23 61  57 74
75   89 21 124 15
644  11 23 373 962
*    +  *   +   *
```

Blocks are separated by columns that are entirely spaces (except for the operator row). Within each block, numbers can span multiple columns and are separated by spaces.

### Input Encoding

Each character gets encoded as a 4-bit value:
- `0-9`: the digit value itself
- `10`: space character
- `11`: `+` operator
- `12`: `*` operator

```ocaml
let encode_char c =
  match c with
  | '0' .. '9' -> Char.get_digit_exn c
  | ' ' -> 10
  | '+' -> 11
  | '*' -> 12
  | _ -> failwith (sprintf "Unknown char: %c" c)
```

Each row is packed as a sequence of 4-bit cells, stored MSB-first (column 0 is at the high bits). The grid dimensions are packed into a single 32-bit value: high 16 bits for width, low 16 bits for height.

### Solver State Machine

The solver (`day06_solver.ml`) implements a 17-state FSM. The high-level flow:

1. Load all rows from RAM into registers
2. Scan right-to-left looking for block boundaries (columns of all spaces)
3. For each block found:
   - Extract the operator from the last row
   - Parse numbers horizontally for Part 1
   - Parse numbers vertically for Part 2
   - Accumulate results

The states in detail:

- **Idle**: Initialize registers, wait for `start` signal, unpack grid dimensions
- **Load_row / Wait_load**: Fetch rows from RAM one by one into internal registers
- **Init_scan / Wait_scan / Scan_delim**: Scan columns right-to-left looking for delimiters (all-space columns)
- **Check_block**: Verify we found a valid block (left < right boundary)
- **Get_op**: Extract operator from the last row at the block's left position
- **P1_row_init / P1_row_parse / P1_row_done**: Parse each row horizontally, extracting multi-digit numbers
- **P1_block_finish**: Add Part 1 result to accumulator, initialize Part 2
- **P2_col_init / P2_col_parse / P2_col_done**: Parse each column vertically
- **P2_block_finish**: Add Part 2 result to accumulator
- **Block_done**: Update boundaries, continue scanning for more blocks
- **Done**: Signal completion, outputs are valid

### Cell Extraction

The trickiest part of this solver is extracting a cell value at a runtime-determined column position. Since we pack cells MSB-first, column 0 is at the high end of the row register:

```ocaml
let get_cell row col =
  (* Pre-extract all possible 4-bit cells at each position from the RIGHT (LSB) *)
  let all_cells =
    List.init max_cols ~f:(fun i -> select row ~high:((i * 4) + 3) ~low:(i * 4))
  in
  (* Position from right = (grid_width - 1 - col) *)
  let pos_from_right = grid_width.value -:. 1 -: uresize col ~width:16 in
  mux pos_from_right all_cells
```

This creates a big mux that selects from all possible cell positions based on the runtime column index. It's not the most area-efficient approach, but it avoids the complexity of barrel shifters.

### Block Detection

To find block boundaries, we scan from right to left looking for "delimiter columns" where all data cells (excluding the operator row) are spaces:

```ocaml
let is_col_delimiter =
  let checks =
    List.init max_rows ~f:(fun r ->
      let row_data = row_regs.(r).value in
      let cell = get_cell row_data scan_col.value in
      (* Either this row is >= data_height (operator row) or cell is space *)
      uresize data_height ~width:8 <=:. r |: (cell ==: space_val))
  in
  List.reduce_exn checks ~f:( &: )
```

### Number Parsing

Both horizontal and vertical parsing follow a similar pattern: accumulate digits into a number, emit when we hit a space (or end of range):

```ocaml
if_ cell_is_digit
  [ (* Digit: accumulate into current number *)
    p1_num <-- sel_bottom (p1_num.value *: ten) ~width:result_width
              +: uresize p1_current_cell ~width:result_width
  ; p1_in_num <--. 1
  ]
  [ (* Space: if we were in a number, emit it *)
    when_ p1_in_num.value
      [ if_ first_num.value
          [ block_acc <-- p1_num.value; first_num <--. 0 ]
          [ block_acc <-- accumulate_num p1_num.value ]
      ; p1_num <--. 0
      ; p1_in_num <--. 0
      ]
  ]
```

The `accumulate_num` helper applies the block's operator:

```ocaml
let accumulate_num num =
  let mul_result = sel_bottom (block_acc.value *: num) ~width:result_width in
  mux2 block_op.value mul_result (block_acc.value +: num)
```

### Registers

We use quite a few registers to track parsing state:

- `grid_width` / `grid_height` (16-bit each): Grid dimensions
- `row_regs` (8 x 14800-bit): Storage for up to 8 rows of data
- `scan_col` / `block_left` / `block_right` (16-bit): Block boundary tracking
- `block_op` (1-bit): Current operator (0 = add, 1 = multiply)
- `block_acc` (64-bit): Accumulator for current block's operation
- `first_num` (1-bit): Flag to handle first number specially (initialize vs accumulate)
- `p1_row` / `p1_col` / `p1_num` / `p1_in_num`: Part 1 parsing state
- `p2_col` / `p2_row` / `p2_num` / `p2_in_num`: Part 2 parsing state
- `part1_acc` / `part2_acc` (64-bit): Final result accumulators

### Vertical vs Horizontal Parsing

The key difference between Part 1 and Part 2 is the parsing direction:

- **Part 1 (horizontal)**: For each row, scan columns left-to-right within block bounds
- **Part 2 (vertical)**: For each column, scan rows top-to-bottom (excluding operator row)

Vertical parsing has a subtle difference: we skip leading spaces but stop as soon as we hit a space after seeing digits. This handles the ragged alignment of numbers in columns.
