open! Core
open! Hardcaml
module Day03_solver = Advent_of_fpga_2025.Day03_solver
module Day03_parser = Advent_of_fpga_2025_input_parser.Day03
module Sim = Cyclesim.With_interface (Day03_solver.I) (Day03_solver.O)

(** Encode a digit string to a 400-bit Bits value. Each digit (1-9) is encoded as 4 bits,
    concatenated MSB first. *)
let encode_line s =
  let digit_bits =
    String.to_list s
    |> List.map ~f:(fun c ->
      let d = Char.get_digit_exn c in
      Bits.of_int_trunc ~width:4 d)
  in
  (* Concatenate MSB first, then zero-extend to data_width *)
  let encoded = Bits.concat_msb digit_bits in
  Bits.uresize ~width:Day03_solver.data_width encoded
;;

let run () =
  let input_lines = Day03_parser.parse "day03.txt" in
  let input_data = Array.of_list_map input_lines ~f:encode_line in
  let input_count = Array.length input_data in
  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day03_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver *)
  inputs.input_count := Bits.of_int_trunc ~width:Day03_solver.addr_bits input_count;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data =
      if addr < input_count then input_data.(addr) else Bits.zero Day03_solver.data_width
    in
    inputs.ram_read_data := data;
    cycle ();
    Int.incr cycles_run
  done;
  let part1 = Bits.to_int_trunc !(outputs.part1) in
  let part2 = Bits.to_int64_trunc !(outputs.part2) in
  printf "Part 1: %d\n" part1;
  printf "Part 2: %Ld\n" part2;
  printf "Cycles: %d\n" !cycles_run
;;

let () = run ()
