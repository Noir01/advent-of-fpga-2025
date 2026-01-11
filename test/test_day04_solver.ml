open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day04_solver = Advent_of_fpga_2025.Day04_solver
module Day04_parser = Advent_of_fpga_2025_input_parser.Day04
module Harness = Cyclesim_harness.Make (Day04_solver.I) (Day04_solver.O)

(** Encode a binary string (e.g., "001101") to a Bits value, zero-extended to data_width.
    The string is MSB-first (leftmost char = MSB). *)
let encode_row s =
  let bits =
    String.to_list s
    |> List.map ~f:(fun c ->
      match c with
      | '0' -> Bits.gnd
      | '1' -> Bits.vdd
      | _ -> failwith (sprintf "Invalid bit char: %c" c))
  in
  let encoded = Bits.concat_msb bits in
  Bits.uresize ~width:Day04_solver.data_width encoded
;;

let sample_input =
  {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}
;;

(* Expected results for sample input *)
let expected_p1 = 13
let expected_p2 = 43

let run_solver ~width ~height rows =
  let input_data = Array.of_list_map rows ~f:encode_row in
  let testbench (sim : Harness.Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle ?n () = Cyclesim.cycle ?n sim in
    (* Pack width (high 8 bits) and height (low 8 bits) into input_count *)
    let packed_dims = (width lsl 8) lor height in
    (* Reset the design *)
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    cycle ();
    (* Set up packed dimensions and start *)
    inputs.input_count := Bits.of_int_trunc ~width:Day04_solver.addr_bits packed_dims;
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
        if addr < num_rows then input_data.(addr) else Bits.zero Day04_solver.data_width
      in
      inputs.ram_read_data := data;
      cycle ();
      Int.incr cycles_run
    done;
    (* Extract results *)
    let part1 = Bits.to_int_trunc !(outputs.part1) in
    let part2 = Bits.to_int_trunc !(outputs.part2) in
    print_s
      [%message
        "Results"
          (part1 : int)
          (part2 : int)
          (!cycles_run : int)
          (expected_p1 : int)
          (expected_p2 : int)]
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day04_solver.hierarchical
    testbench
;;

let%expect_test "Sample test - grid encoding and solver completion" =
  let width, height, rows = Day04_parser.parse_string sample_input in
  print_s [%message "Grid dimensions with border" (width : int) (height : int)];
  run_solver ~width ~height rows;
  [%expect
    {|
    ("Grid dimensions with border" (width 12) (height 12))
    (Results (part1 13) (part2 43) (!cycles_run 792) (expected_p1 13)
     (expected_p2 43))
    |}]
;;
