open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day05_solver = Advent_of_fpga_2025.Day05_solver
module Day05_parser = Advent_of_fpga_2025_input_parser.Day05
module Harness = Cyclesim_harness.Make (Day05_solver.I) (Day05_solver.O)

(** Encode an int64 to a Bits value of data_width bits *)
let encode_value v =
  Bits.of_int64_trunc ~width:Day05_solver.data_width v
;;

(** Build RAM data from parsed input.
    Layout:
    - Address 0: num_ranges
    - Address 1: num_numbers
    - Address 2 to 2+2*num_ranges-1: ranges (start, end pairs)
    - Address 2+2*num_ranges onwards: numbers *)
let build_ram_data ~num_ranges ~num_numbers ~ranges ~numbers =
  let data = ref [] in
  (* Add counts - prepend in reverse order so after List.rev we get correct order *)
  data := encode_value (Int64.of_int num_ranges) :: !data;
  data := encode_value (Int64.of_int num_numbers) :: !data;
  (* Add ranges (start, end pairs) - prepend in reverse order *)
  List.iter ranges ~f:(fun (start_v, end_v) ->
    data := encode_value start_v :: !data;
    data := encode_value end_v :: !data);
  (* Add numbers *)
  List.iter numbers ~f:(fun n ->
    data := encode_value n :: !data);
  Array.of_list (List.rev !data)
;;


let sample_input =
  {|3-5
10-14
16-20
12-18

1
5
8
11
17
32|}
;;

(* Expected results for sample input *)
let expected_p1 = 3
let expected_p2 = 14

let run_solver ~num_ranges ~num_numbers ~ranges ~numbers =
  let input_data = build_ram_data ~num_ranges ~num_numbers ~ranges ~numbers in
  let total_words = Array.length input_data in
  let testbench (sim : Harness.Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle ?n () = Cyclesim.cycle ?n sim in
    (* Reset the design *)
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    cycle ();
    (* Set up input count and start *)
    inputs.input_count := Bits.of_int_trunc ~width:Day05_solver.addr_bits total_words;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    (* Simulate RAM: provide data based on ram_read_addr *)
    let max_cycles = 2000 in
    let cycles_run = ref 0 in
    let ram_data_latency = ref (Bits.zero Day05_solver.data_width) in

    while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
      let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
      let next_data =
        if addr < total_words
        then input_data.(addr)
        else Bits.zero Day05_solver.data_width
      in
      
      (* Simulate 1-cycle RAM latency *)
      inputs.ram_read_data := !ram_data_latency;
      ram_data_latency := next_data;

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
          (!cycles_run : int)]
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day05_solver.hierarchical
    testbench
;;

let%expect_test "Sample test - parsing and solver infrastructure" =
  let num_ranges, num_numbers, ranges, numbers = Day05_parser.parse_string sample_input in
  print_s
    [%message
      "Parsed input"
        (num_ranges : int)
        (num_numbers : int)
        (ranges : (int64 * int64) list)
        (numbers : int64 list)];
  run_solver ~num_ranges ~num_numbers ~ranges ~numbers;
  [%expect
    {|
    ("Parsed input" (num_ranges 4) (num_numbers 6)
     (ranges ((3 5) (10 14) (16 20) (12 18))) (numbers (1 5 8 11 17 32)))
    (Results (part1 3) (part2 14) (!cycles_run 115))
    |}]
;;
