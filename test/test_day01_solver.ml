(** This file was LLM generated, I may try to rewrite this towards the end of the
    challenge. *)

open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day01_solver = Advent_of_fpga_2025.Day01_solver
module Harness = Cyclesim_harness.Make (Day01_solver.I) (Day01_solver.O)

(** Convert direction and distance to 16-bit word. Bit 15 = direction (1=L, 0=R), bits
    14-0 = distance *)
let encode_input dir dist =
  let dir_bit = if Char.equal dir 'L' then 0x8000 else 0 in
  dir_bit lor dist
;;

let sample_input =
  [| encode_input 'L' 68
   ; encode_input 'L' 30
   ; encode_input 'R' 48
   ; encode_input 'L' 5
   ; encode_input 'R' 60
   ; encode_input 'L' 55
   ; encode_input 'L' 1
   ; encode_input 'L' 99
   ; encode_input 'R' 14
   ; encode_input 'L' 82
  |]
;;

let sample_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let input_count = Array.length sample_input in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Set up input count and start *)
  inputs.input_count := Bits.of_int_trunc ~width:Day01_solver.addr_bits input_count;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM: provide data based on ram_read_addr *)
  let max_cycles = 1000000 in
  (* Upper bound based on total ticks *)
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    (* Read the current address and provide corresponding data *)
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data = if addr < input_count then sample_input.(addr) else 0 in
    inputs.ram_read_data := Bits.of_int_trunc ~width:Day01_solver.data_width data;
    cycle ();
    Int.incr cycles_run
  done;
  (* Extract results *)
  let part1 = Bits.to_int_trunc !(outputs.part1) in
  let part2 = Bits.to_int_trunc !(outputs.part2) in
  print_s [%message "Results" (part1 : int) (part2 : int) (!cycles_run : int)]
;;

let%expect_test "Sample input test: L68, L30, R48, L5, R60, L55, L1, L99, R14, L82" =
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day01_solver.hierarchical
    sample_testbench;
  [%expect {| (Results (part1 3) (part2 6) (!cycles_run 493)) |}]
;;

(** Simple test: R50 (ends at 0, crosses 0 once) *)
let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let input_data = [| encode_input 'R' 50 |] in
  let input_count = Array.length input_data in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Set up input count and start *)
  inputs.input_count := Bits.of_int_trunc ~width:Day01_solver.addr_bits input_count;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM *)
  let max_cycles = 1000 in
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
  print_s [%message "Results" (part1 : int) (part2 : int)]
;;

let%expect_test "Simple test: R50 -> ends at 0, crosses 0 once" =
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day01_solver.hierarchical
    simple_testbench;
  [%expect {| (Results (part1 1) (part2 1)) |}]
;;

(* To run on full input with live output: dune exec bin/run_day01.exe *)
