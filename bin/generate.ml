open! Core
open! Hardcaml
open! Advent_of_fpga_2025

(** Generate RTL for the Day01 top module (full UART + solver) *)
let generate_day01_top_rtl () =
  let module Top = Top.Day01_top in
  let module C = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day01_top" (Top.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let day01_top_command =
  Command.basic
    ~summary:"Generate RTL for Day01 top module (UART + solver)"
    [%map_open.Command
      let () = return () in
      fun () -> generate_day01_top_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Generate Verilog RTL for Advent of FPGA designs"
       [ "day01", day01_top_command ])
;;
