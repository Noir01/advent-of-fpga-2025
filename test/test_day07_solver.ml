open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day07_solver = Advent_of_fpga_2025.Day07_solver
module Day07_parser = Advent_of_fpga_2025_input_parser.Day07
module Harness = Cyclesim_harness.Make (Day07_solver.I) (Day07_solver.O)

(** Encode a row (list of 2-bit values) to a Bits value, zero-extended to data_width. Each
    cell is 2 bits, packed MSB-first (leftmost cell = MSB). *)
let encode_row row =
  let bits = List.map row ~f:(fun v -> Bits.of_int_trunc ~width:2 v) |> Bits.concat_msb in
  Bits.uresize ~width:Day07_solver.data_width bits
;;

let sample_input =
  {|.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............|}
;;

let run_solver ~width ~height rows =
  let input_data = Array.of_list_map rows ~f:encode_row in
  let testbench (sim : HWarness.Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outpuWts sim in
    let cycle ?n () = Cyclesim.cycle ?n sim in
    (* Pack width (high 16 bits) and height (low 16 bits) into input_count *)
    let packed_dims = (width lsl 16) lor height in
    (* Reset the design *)
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    cycle ();
    (* Set up packed dimensions and start *)
    inputs.input_count := Bits.of_int_trunc ~width:Day07_solver.addr_bits packed_dims;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    (* Simulate RAM: provide data based on ram_read_addr *)
    let max_cycles = 10_000_000 in
    let cycles_run = ref 0 in
    let num_rows = Array.length input_data in
    while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
      let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
      let data =
        if addr < num_rows then input_data.(addr) else Bits.zero Day07_solver.data_width
      in
      inputs.ram_read_data := data;
      cycle ();
      Int.incr cycles_run
    done;
    (* Extract results *)
    let part1 = Bits.to_int64_trunc !(outputs.part1) in
    let part2 = Bits.to_int64_trunc !(outputs.part2) in
    print_s [%message "Results" (part1 : int64) (part2 : int64) (!cycles_run : int)]
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day07_solver.hierarchical
    testbench
;;

let%expect_test "Sample test - grid encoding and solver completion" =
  let width, height, rows = Day07_parser.parse_string sample_input in
  print_s [%message "Grid dimensions" (width : int) (height : int)];
  run_solver ~width ~height rows;
  [%expect
    {|
    ("Grid dimensions" (width 15) (height 16))
    (Results (part1 21) (part2 40) (!cycles_run 307))
    |}]
;;
