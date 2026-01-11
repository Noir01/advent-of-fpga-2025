open! Core
open! Hardcaml
module Day04_solver = Advent_of_fpga_2025.Day04_solver
module Day04_parser = Advent_of_fpga_2025_input_parser.Day04
module Sim = Cyclesim.With_interface (Day04_solver.I) (Day04_solver.O)

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

let run () =
  let width, height, rows = Day04_parser.parse "day04.txt" in
  printf "Grid dimensions with border: %d x %d\n" width height;
  let input_data = Array.of_list_map rows ~f:encode_row in
  let num_rows = Array.length input_data in
  (* Pack width (high 8 bits) and height (low 8 bits) into input_count *)
  let packed_dims = (width lsl 8) lor height in
  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day04_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver with packed dimensions *)
  inputs.input_count := Bits.of_int_trunc ~width:Day04_solver.addr_bits packed_dims;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data =
      if addr < num_rows then input_data.(addr) else Bits.zero Day04_solver.data_width
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
