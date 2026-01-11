(** Top-level module: Wires together UART, Loader, RAM, and Solver.

    State machine:
    Idle -> (start byte) --> Loading -> (load done) --> Running -> (solver done) --> Output -> (tx done) --> Idle *)

open! Core
open! Hardcaml
open! Signal

module Uart_rx_module = Uart_rx
module Uart_tx_module = Uart_tx
module Loader_module = Loader

module Make (Solver : sig
    val data_width : int
    val addr_bits : int
    val result_width : int

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; input_count : 'a [@bits addr_bits]
        ; ram_read_data : 'a [@bits data_width]
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { ram_read_addr : 'a [@bits addr_bits]
        ; part1 : 'a [@bits result_width]
        ; part2 : 'a [@bits result_width]
        ; done_ : 'a
        ; debug_state : 'a [@bits 6]
        ; debug_insert_pos : 'a [@bits 16]
        ; debug_shift_idx : 'a [@bits 16]
        ; debug_bram_read : 'a [@bits data_width]
        ; debug_cmp_val : 'a [@bits data_width]
        ; debug_write_addr : 'a [@bits addr_bits]
        ; debug_write_data : 'a [@bits data_width]
        ; debug_read_addr : 'a [@bits addr_bits]
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end) =
struct
  let data_width = Solver.data_width
  let addr_bits = Solver.addr_bits
  let result_width = Solver.result_width
  (* Total bytes to transmit: part1 + part2 *)
  let result_bytes = 2 * ((result_width + 7) / 8)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; rx : 'a (** UART receive line *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx : 'a (** UART transmit line *)
      ; state : 'a [@bits 3] (** Current state for debugging *)
      ; led : 'a [@bits 4] (** Seems unnecessary for now *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Loading
      | Running
      | Output
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Uart_rx_inst = Uart_rx_module.Default
  module Uart_tx_inst = Uart_tx_module.Default

  module Loader_inst = Loader_module.Make (struct
      let data_width = data_width
      let addr_bits = addr_bits
      let bytes_per_word = (data_width + 7) / 8
    end)

  module Ram_inst = Input_ram.Make (struct
      let data_width = data_width
      let addr_bits = addr_bits
    end)

  let create scope (i : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    (* UART RX *)
    let uart_rx =
      Uart_rx_inst.hierarchical
        scope
        { Uart_rx_inst.I.clock = i.clock; clear = i.clear; rx = i.rx }
    in
    (* UART TX control *)
    let tx_data = Variable.wire ~default:(zero 8) () in
    let tx_start = Variable.wire ~default:gnd () in
    let uart_tx =
      Uart_tx_inst.hierarchical
        scope
        { Uart_tx_inst.I.clock = i.clock
        ; clear = i.clear
        ; data = tx_data.value
        ; start = tx_start.value
        }
    in
    (* Loader *)
    let loader_start = Variable.wire ~default:gnd () in
    let loader =
      Loader_inst.hierarchical
        scope
        { Loader_inst.I.clock = i.clock
        ; clear = i.clear
        ; uart_byte = uart_rx.data
        ; uart_valid = uart_rx.valid
        ; start = loader_start.value
        }
    in
    (* Forward declare solver read address for RAM *)
    let solver_read_addr = wire addr_bits in
    (* RAM *)
    let ram =
      Ram_inst.hierarchical
        scope
        { Ram_inst.I.clock = i.clock
        ; write_enable = loader.ram_write_enable
        ; write_addr = loader.ram_write_addr
        ; write_data = loader.ram_write_data
        ; read_addr = solver_read_addr
        }
    in
    (* Solver *)
    let solver_start = Variable.wire ~default:gnd () in
    let solver =
      Solver.hierarchical
        scope
        { Solver.I.clock = i.clock
        ; clear = i.clear
        ; start = solver_start.value
        ; input_count = loader.word_count
        ; ram_read_data = ram.read_data
        }
    in
    (* Connect solver output to RAM read address *)
    Signal.assign solver_read_addr solver.ram_read_addr;
    (* Result transmission state - concatenate part1 and part2 *)
    let combined_width = 2 * result_width in
    let%hw_var result_reg = Variable.reg spec ~width:combined_width in
    let%hw_var tx_byte_index = Variable.reg spec ~width:8 in
    let%hw_var tx_pending = Variable.reg spec ~width:1 in
    (* Extract current byte based on tx_byte_index (little-endian) *)
    (* Shift amount = byte_index * 8 *)
    let shift_amount = uresize ~width:7 (sll ~by:3 tx_byte_index.value) in
    let current_byte = sel_bottom ~width:8 (log_shift ~f:srl ~by:shift_amount result_reg.value) in
    compile
      [ sm.switch
          [ ( Idle
            , [ (* Wait for any byte to trigger loading *)
                when_
                  uart_rx.valid
                  [ loader_start <-- vdd; sm.set_next Loading ]
              ] )
          ; ( Loading
            , [ (* Wait for loader to finish *)
                when_ loader.ready [ solver_start <-- vdd; sm.set_next Running ]
              ] )
          ; ( Running
            , [ (* Wait for solver to complete *)
                when_
                  solver.done_
                  [ (* Concatenate part1 (low) and part2 (high) into result_reg *)
                    result_reg <-- concat_msb [ solver.part2; solver.part1 ]
                  ; tx_byte_index <--. 0
                  ; tx_pending <--. 0
                  ; sm.set_next Output
                  ]
              ] )
          ; ( Output
            , [ (* Transmit result bytes one at a time *)
                if_
                  (tx_pending.value ==:. 0)
                  [ if_
                      (tx_byte_index.value >=:. result_bytes)
                      [ sm.set_next Idle ]
                      [ when_
                          ~:(uart_tx.busy)
                          [ tx_data <-- current_byte
                          ; tx_start <-- vdd
                          ; tx_pending <--. 1
                          ]
                      ]
                  ]
                  [ when_
                      uart_tx.done_
                      [ tx_pending <--. 0
                      ; tx_byte_index <-- tx_byte_index.value +:. 1
                      ]
                  ]
              ] )
          ]
      ];
    let state_bits =
      mux
        (sm.current -- "state")
        [ of_int_trunc ~width:3 0 (* Idle *)
        ; of_int_trunc ~width:3 1 (* Loading *)
        ; of_int_trunc ~width:3 2 (* Running *)
        ; of_int_trunc ~width:3 3 (* Output *)
        ]
    in
    let led =
      concat_msb
        [ sm.is Loading (* LED 3: Loading *)
        ; sm.is Running (* LED 2: Running *)
        ; sm.is Output (* LED 1: Output *)
        ; uart_rx.valid (* LED 0: RX activity *)
        ]
    in
    { O.tx = uart_tx.tx; state = state_bits; led }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"top" create
  ;;
end

(** Day 01 top module instantiation *)
module Day01_top = Make (Day01_solver)
