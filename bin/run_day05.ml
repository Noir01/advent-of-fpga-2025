open! Core
open! Hardcaml
module Day05_solver = Advent_of_fpga_2025.Day05_solver
module Day05_parser = Advent_of_fpga_2025_input_parser.Day05
module Sim = Cyclesim.With_interface (Day05_solver.I) (Day05_solver.O)

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

let run () =
  let num_ranges, num_numbers, ranges, numbers = Day05_parser.parse "day05.txt" in
  let input_data = build_ram_data ~num_ranges ~num_numbers ~ranges ~numbers in
  let total_words = Array.length input_data in
  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day05_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver with total word count *)
  inputs.input_count := Bits.of_int_trunc ~width:Day05_solver.addr_bits total_words;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  let ram_data_latency = ref (Bits.zero Day05_solver.data_width) in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let next_data =
      if addr < total_words then input_data.(addr) else Bits.zero Day05_solver.data_width
    in
    inputs.ram_read_data := !ram_data_latency;
    ram_data_latency := next_data;
    cycle ();
    Int.incr cycles_run
  done;
  let part1 = Bits.to_int64_trunc !(outputs.part1) in
  let part2 = Bits.to_int64_trunc !(outputs.part2) in
  printf "Part 1: %Ld\n" part1;
  printf "Part 2: %Ld\n" part2;
  printf "Cycles: %d\n" !cycles_run
;;

let () = run ()
