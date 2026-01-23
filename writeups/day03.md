# SPOILERS!!!
This writeup assumes that you've already read AND solved day 3. If you haven't, you'll probably get spoiled.

### The Problem

The problem: we're given 200 lines, each containing 100 digits (1-9). We need to process each line from left to right and track the largest digits we've seen. The "joltage" for a line is computed by treating the largest digits as a decimal number.

- **Part 1**: Track the 2 largest digits, compute joltage as a 2-digit number, sum across all lines
- **Part 2**: Track the 12 largest digits, compute joltage as a 12-digit number, sum across all lines

The trick is instead of just finding the max, we maintain a sorted buffer of the K largest values seen so far. When a new digit comes in that's larger than or equal to an existing value, it gets inserted at the appropriate position, pushing smaller values down (and the smallest drops off).

### Input Encoding

Each line is packed as 400 bits (100 digits x 4 bits each). The digits are stored with the leftmost digit at the MSB:

```ocaml
let data_width = 400 (* 100 digits x 4 bits each *)
let num_digits = 100
```

The parser simply strips the input lines; the actual bit-packing happens in the test harness since the input_parser module doesn't depend on Hardcaml.

### The Battery Insertion Algorithm

The core algorithm maintains an array of "batteries" (registers storing digits) in sorted order, largest first. When processing a new digit, we need to:

1. Check if the incoming digit should be inserted (i.e., it's >= the current largest)
2. Determine at which position to insert
3. Shift existing values down to make room

This is implemented combinationally with ripple logic:

```ocaml
let compute_new_batteries ~batteries_vals ~incoming =
  let num_batteries = Array.length batteries_vals in
  (* inserted[k] = whether to insert/swap at position k *)
  let inserted =
    Array.init num_batteries ~f:(fun k ->
      if k = 0
      then incoming >=: batteries_vals.(0)
      else (
        let initial_insert = incoming >=: batteries_vals.(0) in
        let ripple_ok =
          Array.foldi batteries_vals ~init:vdd ~f:(fun m acc bat ->
            if m > 0 && m <= k then acc &: (batteries_vals.(m - 1) >=: bat) else acc)
        in
        initial_insert &: ripple_ok))
  in
  Array.init num_batteries ~f:(fun k ->
    if k = 0
    then mux2 inserted.(0) incoming batteries_vals.(0)
    else mux2 inserted.(k) batteries_vals.(k - 1) batteries_vals.(k))
```

The `inserted` array tracks whether the incoming digit propagates down to position k. At each position, we either take the value from the position above (if inserting) or keep the current value.

### Solver State Machine

The solver implements a 12-state FSM:

- **Idle**: Initialize accumulators to 0, wait for `start` signal
- **Load_line**: Issue RAM read for current line
- **Wait_line**: Wait one cycle for RAM latency, latch line data
- **Init_batteries**: Initialize Part 1 batteries with digits 98-99 (rightmost 2) and Part 2 batteries with digits 88-99 (rightmost 12)
- **Process_digit**: Process one digit through both battery arrays in parallel
- **Check_digit_done**: Decrement digit index; if more digits remain, loop back to Process_digit
- **Joltage_p1**: Compute Part 1 joltage and add to accumulator
- **Joltage_p2_init**: Initialize Part 2 joltage computation
- **Joltage_p2_loop**: Iteratively compute Part 2 joltage (12 iterations)
- **Accumulate**: Add Part 2 joltage to accumulator
- **Next_line**: Advance to next line or finish
- **Done**: Signal completion

### Processing Both Parts in Parallel

A clever (hehe) optimization: we process Part 1 and Part 2 simultaneously. Part 1 only needs digits 0-97 (since batteries are initialized with 98-99), while Part 2 needs digits 0-87. We use the same digit stream but conditionally update each battery set:

```ocaml
(* always update Part 1 batteries (we start at digit 97) *)
proc (Array.to_list
       (Array.mapi batteries_p1 ~f:(fun j bat -> bat <-- new_batteries_p1.(j))))
(* update Part 2 batteries only when in their range (digits 0-87) *)
when_ (digit_idx.value <=:. start_digit_idx_p2)
  [ proc (Array.to_list
           (Array.mapi batteries_p2 ~f:(fun j bat -> bat <-- new_batteries_p2.(j)))) ]
```

### Computing Joltage

Part 1's joltage is simple since it's only 2 digits:

```ocaml
let part1_joltage =
  let b0 = uresize batteries_p1.(0).value ~width:8 in
  let b1 = uresize batteries_p1.(1).value ~width:8 in
  let b0_times_10 = sll b0 ~by:3 +: sll b0 ~by:1 in  (* x10 = x8 + x2 *)
  b0_times_10 +: b1
```

Part 2 is a bit more complex since we need to compute a 12-digit number. We use an iterative approach:

```ocaml
(* joltage = joltage * 10 + batteries_p2[joltage_idx] *)
let joltage_times_10 =
  let j = joltage.value in
  sll j ~by:3 +: sll j ~by:1  (* x10 = x8 + x2 *)
in
let next_joltage = joltage_times_10 +: uresize current_battery_for_joltage ~width:48
```

This loop runs 12 times, building up the decimal value one digit at a time.

### Registers

Key registers for this solver:

- `line_idx` (8-bit): Current line being processed
- `digit_idx` (7-bit): Current digit position within line
- `line_data` (400-bit): The packed digits for the current line
- `batteries_p1` (2 x 4-bit): Part 1's sorted largest digits
- `batteries_p2` (12 x 4-bit): Part 2's sorted largest digits
- `joltage_idx` (4-bit): Loop counter for Part 2 joltage computation
- `joltage` (48-bit): Accumulated joltage value during computation
- `part1_acc` / `part2_acc` (64-bit): Final result accumulators

### Digit Extraction

Extracting a digit from the packed line data uses a mux over all 100 possible positions:

```ocaml
let get_digit_from_signal data pos =
  let pos_from_right = of_int_trunc ~width:7 99 -: pos in
  mux pos_from_right
    (List.init 100 ~f:(fun i -> select data ~high:((i * 4) + 3) ~low:(i * 4)))
```

This generates a 100-way mux that selects the appropriate 4-bit slice based on the position index.
