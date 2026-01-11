open! Core
open! Hardcaml
module Day02_solver = Advent_of_fpga_2025.Day02_solver
module Day02_parser = Advent_of_fpga_2025_input_parser.Day02
module Sim = Cyclesim.With_interface (Day02_solver.I) (Day02_solver.O)

let run () =
  let input_data = Day02_parser.parse "day02.txt" |> Array.of_list in
  let input_count = Array.length input_data in
  (* printf "Input count: %d\n" input_count; *)

  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day02_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver *)
  inputs.input_count := Bits.of_int_trunc ~width:Day02_solver.addr_bits input_count;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data = if addr < input_count then input_data.(addr) else 0 in
    inputs.ram_read_data := Bits.of_int_trunc ~width:Day02_solver.data_width data;
    cycle ();
    Int.incr cycles_run
  done;
  let part1 = Bits.to_int_trunc !(outputs.part1) in
  let part2 = Bits.to_int_trunc !(outputs.part2) in
  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2;
  printf "Cycles: %d\n" !cycles_run
;;

let () = run ()
