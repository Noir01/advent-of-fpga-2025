# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OCaml project implementing FPGA-based solutions to Advent of FPGA 2025 challenges using Hardcaml. Generates synthesizable Verilog RTL that runs on FPGA hardware with UART I/O.

## Build Commands

```bash
dune build                           # Build project
dune runtest                         # Run all inline tests
dune exec bin/run_day01.exe          # Simulate day01 solver with inputs/day01.txt
dune exec bin/generate.exe day01     # Generate Verilog RTL for day01
```

## Code Formatting

Uses ocamlformat with `profile=janestreet`. Format with `dune fmt`.

## Architecture

### Source Layout

- `src/common/` - Shared infrastructure: UART RX/TX, loader, input RAM, top-level wrapper
- `src/solvers/` - Day-specific solver circuits (one per challenge)
- `input_parser/` - Converts puzzle input text to simulation-friendly format
- `bin/` - Executables: `run_day{DD}.ml` (simulation), `generate.ml` (RTL output)
- `test/` - Inline expect tests using `hardcaml_test_harness`

### Solver Interface Pattern

Each solver implements `Solver_intf.S` with configurable parameters:

```ocaml
module Interfaces = Solver_intf.Make_interfaces (struct
  let data_width = 16    (* RAM word width *)
  let addr_bits = 13     (* RAM address bits *)
  let result_width = 64  (* Output width *)
end)
```

Solvers receive input via RAM read port and produce `part1`/`part2` results with a `done_` signal.

### Top Module Composition

`Top.Make(Solver)` wires the complete system:
```
UART RX → Loader → RAM → Solver → UART TX
```

State flow: `Idle → Loading → Running → Output → Idle`

### Testing Pattern

Tests use `Cyclesim_harness` with simulated RAM providing input data:
```ocaml
let%expect_test "test name" =
  Harness.run_advanced ~waves_config:Waves_config.no_waves
    ~create:DayXX_solver.hierarchical testbench;
  [%expect {| expected output |}]
```

## Adding a New Day

1. Create solver in `src/solvers/day{DD}_solver.ml` implementing `Solver_intf.S`
2. Add input parser in `input_parser/day{DD}.ml`
3. Create runner in `bin/run_day{DD}.ml`
4. Add test in `test/test_day{DD}_solver.ml`
5. Update `bin/dune` to include new executable
