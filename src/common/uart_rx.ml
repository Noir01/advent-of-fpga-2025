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
      ; rx : 'a (** Serial input line *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a [@bits 8] (** Received byte *)
      ; valid : 'a (** High for one cycle when byte is ready *)
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
    (* Synchronize RX input to avoid metastability *)
    let rx_sync = reg spec (reg spec i.rx) in
    (* Bit counter: counts clocks within each bit period *)
    let%hw_var bit_counter = Variable.reg spec ~width:counter_bits in
    (* Bit index: which data bit we're receiving (0-7) *)
    let%hw_var bit_index = Variable.reg spec ~width:3 in
    (* Shift register for received data *)
    let%hw_var shift_reg = Variable.reg spec ~width:8 in
    (* Output valid pulse *)
    let valid = Variable.wire ~default:gnd () in
    (* Sample point is in the middle of each bit *)
    let half_bit = clocks_per_bit / 2 in
    let at_sample_point = bit_counter.value ==:. half_bit in
    let at_bit_end = bit_counter.value ==:. clocks_per_bit - 1 in
    compile
      [ sm.switch
          [ ( Idle
            , [ (* Wait for start bit (RX goes low) *)
                bit_counter <--. 0
              ; bit_index <--. 0
              ; when_ (rx_sync ==:. 0) [ sm.set_next Start_bit ]
              ] )
          ; ( Start_bit
            , [ bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_sample_point
                  [ (* Verify start bit is still low *)
                    if_
                      (rx_sync ==:. 0)
                      [ (* Valid start bit, continue *) ]
                      [ (* False start, go back to idle *) sm.set_next Idle ]
                  ]
              ; when_ at_bit_end [ bit_counter <--. 0; sm.set_next Data_bits ]
              ] )
          ; ( Data_bits
            , [ bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_sample_point
                  [ (* Sample data bit (LSB first) *)
                    shift_reg <-- rx_sync @: sel_top ~width:7 shift_reg.value
                  ]
              ; when_
                  at_bit_end
                  [ bit_counter <--. 0
                  ; if_
                      (bit_index.value ==:. 7)
                      [ (* All 8 bits received *) sm.set_next Stop_bit ]
                      [ bit_index <-- bit_index.value +:. 1 ]
                  ]
              ] )
          ; ( Stop_bit
            , [ bit_counter <-- bit_counter.value +:. 1
              ; when_
                  at_sample_point
                  [ (* Check stop bit is high (valid frame) *)
                    when_ (rx_sync ==:. 1) [ valid <-- vdd ]
                  ]
              ; when_ at_bit_end [ sm.set_next Idle ]
              ] )
          ]
      ];
    { O.data = shift_reg.value; valid = valid.value }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"uart_rx" create
  ;;
end

module Default = Make (struct
    let clocks_per_bit = Config.Config.clocks_per_bit
  end)
