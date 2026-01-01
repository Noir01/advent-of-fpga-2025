open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day02_solver = Advent_of_fpga_2025.Day02_solver
module Harness = Cyclesim_harness.Make (Day02_solver.I) (Day02_solver.O)

let run_solver input_data =
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
    inputs.input_count := Bits.of_int_trunc ~width:Day02_solver.addr_bits input_count;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    (* Simulate RAM: provide data based on ram_read_addr *)
    let max_cycles = 1000000 in
    let cycles_run = ref 0 in
    while (not (Bits.to_bool !(outputs.done_))) && !cycles_run < max_cycles do
      let addr = Bits.to_int_trunc !(outputs.ram_read_addr) in
      let data = if addr < input_count then input_data.(addr) else 0L in
      inputs.ram_read_data := Bits.of_int64_trunc ~width:Day02_solver.data_width data;
      cycle ();
      Int.incr cycles_run
    done;
    (* Extract results *)
    let part1 = Bits.to_int64_trunc !(outputs.part1) in
    let part2 = Bits.to_int64_trunc !(outputs.part2) in
    print_s [%message "Results" (part1 : int64) (part2 : int64) (!cycles_run : int)]
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day02_solver.hierarchical
    testbench
;;

let%expect_test "Sample test" =
  run_solver
    [| 11L
     ; 22L
     ; 95L
     ; 115L
     ; 998L
     ; 1012L
     ; 1188511880L
     ; 1188511890L
     ; 222220L
     ; 222224L
     ; 1698522L
     ; 1698528L
     ; 446443L
     ; 446449L
     ; 38593856L
     ; 38593862L
     ; 565653L
     ; 565659L
     ; 824824821L
     ; 824824827L
     ; 2121212118L
     ; 2121212124L
    |];
  [%expect {| (Results (part1 1227775554) (part2 4174379265) (!cycles_run 1304)) |}]
;;
