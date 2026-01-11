open! Core
open! Hardcaml
module Day06_solver = Advent_of_fpga_2025.Day06_solver
module Day06_parser = Advent_of_fpga_2025_input_parser.Day06
module Sim = Cyclesim.With_interface (Day06_solver.I) (Day06_solver.O)

(** Encode a row (list of 4-bit values) to a Bits value, zero-extended to data_width. Each
    cell is 4 bits, packed MSB-first (leftmost cell = MSB). *)
let encode_row row =
  let bits = List.map row ~f:(fun v -> Bits.of_int_trunc ~width:4 v) |> Bits.concat_msb in
  Bits.uresize ~width:Day06_solver.data_width bits
;;

let run () =
  let width, height, rows = Day06_parser.parse "day06.txt" in
  printf "Grid dimensions: %d x %d\n" width height;
  let input_data = Array.of_list_map rows ~f:encode_row in
  let num_rows = Array.length input_data in
  (* Pack width (high 16 bits) and height (low 16 bits) into input_count
     Note: using 16-bit packing since we have large grids *)
  let packed_dims = (width lsl 16) lor height in
  (* Create simulation *)
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Day06_solver.hierarchical scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Start solver with packed dimensions *)
  inputs.input_count := Bits.of_int_trunc ~width:Day06_solver.addr_bits packed_dims;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Simulate RAM and run to completion *)
  let max_cycles = 10_000_000 in
  let cycles_run = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
    let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
    let data =
      if addr < num_rows then input_data.(addr) else Bits.zero Day06_solver.data_width
    in
    inputs.ram_read_data := data;
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
