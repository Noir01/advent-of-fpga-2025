(** Test wrapper for UART-based simulation.

    Provides helpers for feeding bytes through the UART interface
    and capturing the output. *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Top = Advent_of_fpga_2025.Top.Day01_top
module Sim = Cyclesim.With_interface (Top.I) (Top.O)

type t =
  { sim : Sim.t
  ; recv_buffer : char Queue.t
  ; clocks_per_bit : int
  }

(** Create a test wrapper for the Day01 Top module.

    For fast testing, uses a short clocks_per_bit value. *)
let create ?waves ?(clocks_per_bit = 8) () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let config =
    Cyclesim.Config.(
      add_random_initialization trace_all Random_initializer.(create randomize_all))
  in
  (* Create a fast-UART version of the top module for testing *)
  let module Fast_uart_rx = Advent_of_fpga_2025.Uart_rx.Make (struct
      let clocks_per_bit = clocks_per_bit
    end)
  in
  let module Fast_uart_tx = Advent_of_fpga_2025.Uart_tx.Make (struct
      let clocks_per_bit = clocks_per_bit
    end)
  in
  (* For now, use the default Top module - we can parameterize later *)
  let sim = Sim.create ~config (Top.hierarchical scope) in
  let waves, sim =
    match waves with
    | None -> None, sim
    | Some _ ->
      let waves, sim = Waveform.create sim in
      Some waves, sim
  in
  Core.at_exit (fun () ->
    Option.iter waves ~f:(fun waves ->
      Waveform.Serialize.marshall waves "/tmp/waves.hardcamlwaveform"));
  let i = Cyclesim.inputs sim in
  i.rx := Bits.vdd;
  (* UART idle is high *)
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  (* Use actual clocks_per_bit from config for the default module *)
  { sim; recv_buffer = Queue.create (); clocks_per_bit = Advent_of_fpga_2025.Config.Config.clocks_per_bit }
;;

(** Cycle the simulation n times, capturing any UART output *)
let cycle ?(n = 1) { sim; recv_buffer; clocks_per_bit } =
  let o = Cyclesim.outputs sim in
  let _i = Cyclesim.inputs sim in
  (* Simple UART RX state machine for capturing output *)
  let rx_state = ref `Idle in
  let rx_counter = ref 0 in
  let rx_bit_index = ref 0 in
  let rx_byte = ref 0 in
  let sample_point = clocks_per_bit / 2 in
  for _ = 1 to n do
    Cyclesim.cycle sim;
    let tx = Bits.to_bool !(o.tx) in
    (match !rx_state with
     | `Idle ->
       if not tx
       then (
         (* Start bit detected *)
         rx_state := `Start;
         rx_counter := 0;
         rx_bit_index := 0;
         rx_byte := 0)
     | `Start ->
       Int.incr rx_counter;
       if !rx_counter >= clocks_per_bit
       then (
         rx_state := `Data;
         rx_counter := 0)
     | `Data ->
       Int.incr rx_counter;
       if !rx_counter = sample_point
       then
         if tx
         then rx_byte := !rx_byte lor (1 lsl !rx_bit_index);
       if !rx_counter >= clocks_per_bit
       then (
         rx_counter := 0;
         Int.incr rx_bit_index;
         if !rx_bit_index >= 8
         then rx_state := `Stop)
     | `Stop ->
       Int.incr rx_counter;
       if !rx_counter >= clocks_per_bit
       then (
         Queue.enqueue recv_buffer (Char.of_int_exn !rx_byte);
         rx_state := `Idle))
  done
;;

(** Send a single byte over UART (bit-by-bit simulation) *)
let write_byte t byte =
  let i = Cyclesim.inputs t.sim in
  let byte_val = Char.to_int byte in
  (* Send start bit (low) *)
  i.rx := Bits.gnd;
  for _ = 1 to t.clocks_per_bit do
    cycle t
  done;
  (* Send 8 data bits (LSB first) *)
  for bit_idx = 0 to 7 do
    let bit_val = (byte_val lsr bit_idx) land 1 in
    i.rx := Bits.of_bool (bit_val = 1);
    for _ = 1 to t.clocks_per_bit do
      cycle t
    done
  done;
  (* Send stop bit (high) *)
  i.rx := Bits.vdd;
  for _ = 1 to t.clocks_per_bit do
    cycle t
  done
;;

(** Feed a list of UART symbols to the simulation *)
let feed_inputs t (inputs : Advent_of_fpga_2025_input_parser.Util.Uart_symbol.t list) =
  List.iter inputs ~f:(function
    | Byte b -> write_byte t b
    | Rts _ -> ())
;;

(** Get all received UART output as a string *)
let get_uart_output t = t.recv_buffer |> Queue.to_list |> String.of_char_list

(** Print UART output to stdout *)
let dump_uart_output t = get_uart_output t |> print_endline

(** Read memory contents by name *)
let read_memory t name =
  Cyclesim.lookup_mem_by_name t.sim name |> Option.value_exn |> Cyclesim.Memory.read_all
;;

(** Read memory contents as integers *)
let read_memory_int t name = read_memory t name |> Array.map ~f:Bits.to_int_trunc
