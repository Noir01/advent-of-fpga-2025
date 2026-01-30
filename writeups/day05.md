# SPOILERS!!!
This writeup assumes that you've already read AND solved day 5. If you haven't, you'll probably get spoiled.

### The Problem

We're given a list of ranges (e.g., `390522922084641-390522922084641`) and a list of "ingredients" (individual numbers). We need to:

- **Part 1**: Count how many ingredients fall within any of the ranges
- **Part 2**: Compute the total count of valid numbers (sum of all merged range sizes)

The first obvious catch is that ranges can overlap or be adjacent, so we need to merge them first to avoid double-counting.

### The Algorithm

The solution requires three phases:

1. **Sort ranges by start value** - enables efficient merging
2. **Merge overlapping/adjacent ranges** - if range B's start is <= range A's end + 1, they merge
3. **Check each ingredient** - binary or linear search through merged ranges

### Input Encoding

The parser (`day05.ml`) reads ranges as `start-end` pairs, separated from ingredients by a blank line. Each value is stored as a 54-bit integer (since values can go up to 10^16).

```ocaml
(* Parser splits at empty line, then parses ranges and numbers *)
let range_lines, number_lines = split_at_empty [] lines in
let ranges = List.map range_lines ~f:(fun line ->
  match String.lsplit2 line ~on:'-' with
  | Some (start_s, end_s) -> Int64.of_string start_s, Int64.of_string end_s
  | None -> failwith "Invalid range format")
```

### Solver State Machine

The solver (`day05_solver.ml`) implements a 26-state FSM. This is our most complex state machine yet, handling three distinct phases in hardware.

The high-level flow:

1. **Load counts**: Read metadata (number of ranges, number of ingredients)
2. **Load and sort ranges**: Insertion sort as we load each range into BRAM
3. **Load ingredients**: Copy ingredients to a separate BRAM
4. **Merge ranges**: Single pass to merge overlapping/adjacent ranges
5. **Check ingredients**: For each ingredient, scan merged ranges

### BRAMs: On-Chip Memory

This solver uses three Block RAMs (BRAMs):

- **BRAM A** (512 entries): Stores sorted ranges as consecutive (start, end) pairs
- **BRAM B** (512 entries): Stores merged ranges
- **Ingredient BRAM** (1024 entries): Stores ingredients

Unlike external RAM which has unpredictable latency, BRAM gives us single-cycle reads (after one cycle of address setup). The tradeoff is limited size.

```ocaml
let bram_a_read_data =
  Ram.create
    ~collision_mode:Read_before_write
    ~size:512
    ~write_ports:[| { write_clock = i.clock
                    ; write_enable = bram_a_write_enable.value
                    ; write_address = bram_a_write_addr.value
                    ; write_data = bram_a_write_data.value } |]
    ~read_ports:[| { read_clock = i.clock
                   ; read_enable = vdd
                   ; read_address = bram_a_read_addr_wire.value } |]
    ()
```

### Insertion Sort in Hardware

Rather than loading all ranges then sorting, we sort inline as we load. For each new range:

1. Scan forward through sorted ranges to find insertion point
2. Shift all ranges after that point one position right
3. Insert the new range

```ocaml
Insert_find,
  [ if_ (new_start.value <+ bram_a_read_data.(0))
      [ (* Found insertion point - insert at shift_idx *)
        insert_pos <-- shift_idx.value
      ; shift_idx <-- range_idx.value -:. 1
      ; sm.set_next Insert_shift_read ]
      [ (* Continue searching forward *)
        incr shift_idx
      ; if_ (shift_idx.value +:. 1 >=+ range_idx.value)
          [ insert_pos <-- range_idx.value
          ; sm.set_next Insert_write_start ]
          [ bram_a_read_addr <-- (* next range address *)
          ; sm.set_next Insert_find_wait ] ] ]
```

The states for insertion sort:

- **Insert_init**: Initialize search from position 0
- **Insert_find / Insert_find_wait**: Forward scan comparing new_start with each range's start
- **Insert_shift_read / Insert_shift_write**: Shift ranges right to make room
- **Insert_write_start / Insert_write_end**: Write the new range at its sorted position

### Merging Ranges

Once sorted, we do a single pass to merge overlapping or adjacent ranges. Two ranges merge if `new_start <= merge_end + 1`:

```ocaml
Merge_process,
  [ if_ (new_start.value <=+ merge_end.value +:. 1)
      [ (* Overlapping or adjacent - extend current range *)
        merge_end <-- mux2 (bram_a_read_data.(0) >+ merge_end.value)
                           bram_a_read_data.(0)
                           merge_end.value
      ; incr range_idx
      ; (* continue to next range *) ]
      [ (* Gap - emit current merged range, start new one *)
        sm.set_next Merge_emit ] ]
```

As we emit merged ranges, we accumulate Part 2 (total span):

```ocaml
part2_acc <-- part2_acc.value +: uresize
  (mux2 (merge_end.value >=+ merge_start.value)
        (merge_end.value -: merge_start.value +:. 1)
        (zero data_width))
  ~width:result_width
```

### Checking Ingredients

For each ingredient, we scan through merged ranges checking if `start <= ingredient <= end`:

```ocaml
Check_compare,
  [ if_ (current_ingredient.value >=+ check_start.value
         &: (current_ingredient.value <=+ bram_b_read_data.(0)))
      [ ingredient_matched <--. 1
      ; sm.set_next Check_next ]
      [ (* Try next range *)
        incr range_idx
        (* ... *) ] ]
```

When an ingredient matches, we increment Part 1.

### Key States Summary

**Loading phase:**
- **Idle**: Initialize, wait for start
- **Load_counts_req/wait/num_ranges/num_numbers**: Read metadata from RAM
- **Load_range_start/end**: Fetch ranges from external RAM
- **Insert_init/find/find_wait/shift_read/shift_write/write_start/write_end**: Insertion sort
- **Load_range_next**: Advance to next range
- **Load_ingredients_req/write**: Copy ingredients to BRAM

**Merge phase:**
- **Merge_init**: Setup merge pass
- **Merge_read_start/read_end/process**: Read and compare ranges
- **Merge_emit/emit_end**: Output a merged range to BRAM B
- **Merge_final/final_end**: Emit the last merged range

**Check phase:**
- **Check_init**: Setup ingredient checking
- **Check_read_ingr/wait_ingr**: Read ingredient from BRAM
- **Check_read_range_start/range_end/compare**: Compare with ranges
- **Check_next**: Move to next ingredient
- **Done**: Output results

### Registers

Key registers for this solver:

- `num_ranges` / `num_ingredients` (16-bit): Counts from input
- `merged_count` (16-bit): Number of merged ranges produced
- `range_idx` / `ingredient_idx` (16-bit): Loop indices
- `insert_pos` / `shift_idx` (16-bit): Insertion sort state
- `new_start` / `new_end` (54-bit): Current range being inserted
- `merge_start` / `merge_end` (54-bit): Current merged range bounds
- `current_ingredient` / `check_start` (54-bit): Ingredient checking state
- `part1_acc` / `part2_acc` (64-bit): Final results
