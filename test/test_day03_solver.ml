open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day03_solver = Advent_of_fpga_2025.Day03_solver
module Harness = Cyclesim_harness.Make (Day03_solver.I) (Day03_solver.O)

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

let run_solver input_lines =
  let input_data = Array.of_list_map input_lines ~f:encode_line in
  let testbench (sim : Harness.Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle ?n () = Cyclesim.cycle ?n sim in
    let input_count = Array.length input_data in
    (* Reset the design *)
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    cycle ();
    (* Set up input count and start *)
    inputs.input_count := Bits.of_int_trunc ~width:Day03_solver.addr_bits input_count;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    (* Simulate RAM: provide data based on ram_read_addr *)
    let max_cycles = 1000000 in
    let cycles_run = ref 0 in
    while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
      let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
      let data =
        if addr < input_count
        then input_data.(addr)
        else Bits.zero Day03_solver.data_width
      in
      inputs.ram_read_data := data;
      cycle ();
      Int.incr cycles_run
    done;
    (* Extract results *)
    let part1 = Bits.to_int_trunc !(outputs.part1) in
    let part2 = Bits.to_int64_trunc !(outputs.part2) in
    print_s [%message "Results" (part1 : int) (part2 : int64) (!cycles_run : int)]
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day03_solver.hierarchical
    testbench
;;

let%expect_test "Sample test" =
  run_solver
    [ "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" ];
  [%expect {| (Results (part1 357) (part2 3121910778619) (!cycles_run 860)) |}]
;;
