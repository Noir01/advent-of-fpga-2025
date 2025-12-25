open! Core
open! Hardcaml
open! Signal

module Make (Cfg : sig
    val clocks_per_bit : int
  end) =
struct
  let clocks_per_bit = Cfg.clocks_per_bit
  let counter_bits = Int.ceil_log2 (clocks_per_bit + 1)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data : 'a [@bits 8] (** Byte to transmit *)
      ; start : 'a (** Begin transmission *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx : 'a (** Serial output line *)
      ; busy : 'a (** High while transmitting *)
      ; done_ : 'a (** High for one cycle when complete *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Start_bit
      | Data_bits
      | Stop_bit
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let _ = scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    (* Bit counter: counts clocks within each bit period *)
    let%hw_var bit_counter = Variable.reg spec ~width:counter_bits in
    (* Bit index: which data bit we're transmitting (0-7) *)
    let%hw_var bit_index = Variable.reg spec ~width:3 in
    (* Shift register holding data to transmit *)
    let%hw_var shift_reg = Variable.reg spec ~width:8 in
    (* TX output (idle high) *)
    let tx_out = Variable.wire ~default:vdd () in
    (* Done pulse *)
    let done_out = Variable.wire ~default:gnd () in
    let at_bit_end = bit_counter.value ==:. (clocks_per_bit - 1) in
    compile
      [ sm.switch
          [ ( Idle
            , [ tx_out <-- vdd (* Idle high *)
              ; when_
                  i.start
                  [ shift_reg <-- i.data
                  ; bit_counter <--. 0
                  ; bit_index <--. 0
                  ; sm.set_next Start_bit
                  ]
              ] )
          ; ( Start_bit
            , [ tx_out <-- gnd (* Start bit is low *)
              ; bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_bit_end
                  [ bit_counter <--. 0; sm.set_next Data_bits ]
              ] )
          ; ( Data_bits
            , [ (* Output LSB of shift register *)
                tx_out <-- lsb shift_reg.value
              ; bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_bit_end
                  [ bit_counter <--. 0
                  ; (* Shift right for next bit *)
                    shift_reg <-- srl ~by:1 shift_reg.value
                  ; if_
                      (bit_index.value ==:. 7)
                      [ sm.set_next Stop_bit ]
                      [ bit_index <-- bit_index.value +:. 1 ]
                  ]
              ] )
          ; ( Stop_bit
            , [ tx_out <-- vdd (* Stop bit is high *)
              ; bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_bit_end
                  [ done_out <-- vdd; sm.set_next Idle ]
              ] )
          ]
      ];
    let busy = ~:(sm.is Idle) in
    { O.tx = tx_out.value; busy; done_ = done_out.value }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"uart_tx" create
  ;;
end

(** Default UART TX with standard config *)
module Default = Make (struct
    let clocks_per_bit = Config.Config.clocks_per_bit
  end)
