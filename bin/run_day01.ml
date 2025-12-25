open! Core
open! Hardcaml
module Day01_solver = Advent_of_fpga_2025.Day01_solver
module Sim = Cyclesim.With_interface (Day01_solver.I) (Day01_solver.O)

let encode_input dir dist =
  let dir_bit = if Char.equal dir 'L' then 0x8000 else 0 in
  dir_bit lor dist
;;

let run () =
  let raw = In_channel.read_all "inputs/day01.txt" in
  let lines = String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  let input_data =
    List.map lines ~f:(fun line ->
      let line = String.strip line in
      let dir = String.get line 0 in
      let dist = Int.of_string (String.sub line ~pos:1 ~len:(String.length line - 1)) in
      encode_input dir dist)
    |> Array.of_list
  in
  let input_count = Array.length input_data in
  (* printf "Input count: %d\n" input_count; *)

  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day01_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver *)
  inputs.input_count := Bits.of_int_trunc ~width:Day01_solver.addr_bits input_count;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data = if addr < input_count then input_data.(addr) else 0 in
    inputs.ram_read_data := Bits.of_int_trunc ~width:Day01_solver.data_width data;
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
