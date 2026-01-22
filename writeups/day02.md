# SPOILERS!!!
This writeup assumes that you've already read AND solved day 2. If you haven't, you'll probably get spoiled.

### The Problem

To restate the problem without the fluff: we're given a list of ranges (e.g., `9191896883-9191940271`) and need to find all "invalid IDs" within them. An invalid ID is any number formed by repeating a digit pattern, like:
- `1212` (pattern `12` repeated twice)
- `123123` (pattern `123` repeated twice)
- `111111` (pattern `1` repeated six times, OR `11` repeated three times, OR `111` repeated twice)

We need to compute:
- **Part 1**: Sum of invalid IDs where the repetition count is even (i.e., length divisible by 2)
- **Part 2**: Sum of ALL invalid IDs (using inclusion-exclusion to avoid double-counting)

### The Math

The key insight is that we can generate all "repeating pattern" numbers using a formula. For a number with `len` digits formed by repeating a `digits`-length pattern:

```
f = (10^len - 1) / (10^digits - 1)
```

For example:
- `len=4, digits=2`: f = 9999/99 = 101, which generates 101, 202, 303, ..., 9999
- `len=6, digits=3`: f = 999999/999 = 1001, which generates 1001, 2002, ..., 999999

The valid values of `digits` for a given `len` are exactly `len / p` where `p` is a prime factor of `len`. This is why we care about prime factorizations:
- Length 6 = 2 × 3, so we have `digits=3` (from prime 2) and `digits=2` (from prime 3)
- Length 10 = 2 × 5, so we have `digits=5` (from prime 2) and `digits=2` (from prime 5)

For lengths with two prime factors, we'd double-count the "1-digit patterns" (like `111111`), so we subtract them using inclusion-exclusion.

### Input Encoding

Each range is stored as two consecutive 34-bit integers (start, end). Since values can go up to 10^10, we need ceil(log_2(10^10)) = 34 bits.

### Solver State Machine

The solver (`day02_solver.ml`) implements a 17-state FSM that processes 4 ranges in parallel. The parallelism is crucial here since the algorithm requires hardware division, which is slow (iterative).

The high-level flow:

1. **Load a batch**: Read 8 words from RAM (4 start/end pairs)
2. **For each digit length** (2 through 10):
   - Compute the `f` value for each prime factor
   - Divide to find bounds `a` and `b` (the range of multipliers)
   - Accumulate the arithmetic series sum
3. **Reduce**: Sum the 4 parallel accumulators
4. **Repeat** for the next batch

The states in detail:
- **Idle**: Initialize accumulators, wait for `start`
- **Load_batch / Wait_load**: Read 8 words into 4 (start, end) register pairs
- **Par_compute_len**: Compute digit lengths of start and end values
- **Par_check_len**: Check if current length exceeds end length, or skip length 1
- **Par_init_prime / Par_load_f**: Load the `f` value from a lookup table
- **Par_compute_bounds**: Set up division for `a = ceil(effective_start / f)`
- **Par_div_a_start / Par_div_a_wait**: Wait for all 4 dividers to compute `a`
- **Par_div_b_start / Par_div_b_wait**: Wait for all 4 dividers to compute `b`
- **Par_compute_terms**: Compute `f * (b(b+1) - a(a-1)) / 2` and accumulate
- **Par_next_prime**: Advance to next prime factor or next length
- **Next_batch**: Load next 4 ranges or finish
- **Reduce / Done**: Sum partial results, signal completion

### Lookup Tables

Rather than computing `f` values on the fly, we precompute them:

```ocaml
let f_values =
  [| (* len=2, prime=2, digits=1 *) [| 11L; 0L; 0L |]
   ; (* len=3, prime=3, digits=1 *) [| 111L; 0L; 0L |]
   ; (* len=4, prime=2, digits=2 *) [| 101L; 0L; 0L |]
   ; (* len=5, prime=5, digits=1 *) [| 11111L; 0L; 0L |]
   ; (* len=6: prime0=2, prime1=3 *) [| 1001L; 10101L; 111111L |]
   ...
  |]
```

For lengths with two prime factors (6 and 10), we store three values: `f` for prime0, `f` for prime1, and `f` for the exclusion term (the all-ones repunit).

### Hardware Division

The trickiest part of this solver is the division. We need to compute:
- `a = ceil(effective_start / f)`
- `b = floor(effective_end / f)`

Hardcaml provides an iterative divider, which takes multiple cycles but uses minimal area. We instantiate 4 of them to match our 4-way parallelism:

```ocaml
module Divider = Divider.Make (struct
    let width = 64
    let signedness = Signedness.Unsigned
    let architecture = Divider.Architecture.Iterative
  end)
```

The FSM waits for all 4 dividers to finish before proceeding, tracked by the `all_dividers_done` signal.

### Registers

We have quite a few registers for this one:
- `read_addr` (8-bit): RAM address pointer
- `load_word_idx` (4-bit): Tracks which of the 8 words we're loading
- `part1_accum` / `part2_accum` (128-bit): Final accumulators

And for each of the 4 parallel units:
- `range_start` / `range_end` (34-bit): The current range bounds
- `len_end` / `current_len` (4-bit): Digit length tracking
- `prime_idx` (2-bit): Which prime factor we're processing (0, 1, or 2 for exclusion)
- `f_value` (34-bit): Current `f` from lookup table
- `effective_start` / `effective_end` (34-bit): Clamped bounds for current length
- `a_value` / `b_value` (34-bit): Division results
- `part1_local` / `part2_local` (128-bit): Per-unit accumulators

### The Series Formula

Once we have `a` and `b`, computing the sum of all `k * f` for `a <= k <= b` is an arithmetic series:

```ocaml
let series_result =
  Array.mapi a_value ~f:(fun idx a ->
    let b = b_value.(idx) in
    let f = f_value.(idx) in
    (* f * (b(b+1) - a(a-1)) / 2 *)
    let term_b = b_ext *: b_plus_1 in
    let term_a = a_ext *: a_minus_1 in
    let diff = term_b -: term_a in
    let product = f_ext *: diff_ext in
    srl product ~by:1)  (* divide by 2 *)
```

The `srl ~by:1` is a right shift, which is how we divide by 2 in hardware.
