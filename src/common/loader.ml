(** Loader: Assembles incoming UART bytes into words and writes to RAM.

    Protocol: [START: 0x01] [COUNT_LO] [COUNT_HI] [DATA bytes...] [END: 0xFF]

    Assembles bytes into data_width-bit words (little-endian). *)

open! Core
open! Hardcaml
open! Signal

module Make (Cfg : Config.Day_config) = struct
  let data_width = Cfg.data_width
  let addr_bits = Cfg.addr_bits
  let bytes_per_word = Cfg.bytes_per_word
  let byte_index_bits = Int.ceil_log2 (max bytes_per_word 1)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_byte : 'a [@bits 8]
      ; uart_valid : 'a
      ; start : 'a (** Reset and prepare for new load *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ram_write_enable : 'a
      ; ram_write_addr : 'a [@bits addr_bits]
      ; ram_write_data : 'a [@bits data_width]
      ; word_count : 'a [@bits addr_bits] (** Total words loaded *)
      ; ready : 'a (** All data loaded, ready to run solver *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Wait_start (** Waiting for START marker (0x01) *)
      | Read_count_lo (** Reading low byte of word count *)
      | Read_count_hi (** Reading high byte of word count *)
      | Read_data (** Reading data bytes *)
      | Done (** Loading complete *)
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let _ = scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    (* Expected word count (from header) *)
    let%hw_var expected_words = Variable.reg spec ~width:16 in
    (* Current word being assembled *)
    let%hw_var current_word = Variable.reg spec ~width:data_width in
    (* Byte index within current word (0 to bytes_per_word-1) *)
    let%hw_var byte_index = Variable.reg spec ~width:(max byte_index_bits 1) in
    (* RAM write address (word address) *)
    let%hw_var write_addr = Variable.reg spec ~width:addr_bits in
    (* Count of words written *)
    let%hw_var words_written = Variable.reg spec ~width:addr_bits in
    (* RAM write signals *)
    let ram_we = Variable.wire ~default:gnd () in
    let ram_wdata = Variable.wire ~default:(zero data_width) () in
    let start_marker = Config.Config.start_marker in
    let end_marker = Config.Config.end_marker in
    (* Pre-compute signals used in state machine *)
    let byte_as_16 = uresize ~width:16 i.uart_byte in
    let hi_byte = sll ~by:8 byte_as_16 in
    let byte_extended = uresize ~width:data_width i.uart_byte in
    (* For bytes_per_word=2: byte_index is 0 or 1, shift by 0 or 8 bits *)
    let shifted_byte =
      mux2 (byte_index.value ==:. 0) byte_extended (sll ~by:8 byte_extended)
    in
    let new_word = current_word.value |: shifted_byte in
    compile
      [ sm.switch
          [ ( Idle
            , [ when_
                  i.start
                  [ expected_words <--. 0
                  ; current_word <--. 0
                  ; byte_index <--. 0
                  ; write_addr <--. 0
                  ; words_written <--. 0
                  ; sm.set_next Wait_start
                  ]
              ] )
          ; ( Wait_start
            , [ when_
                  i.uart_valid
                  [ when_ (i.uart_byte ==:. start_marker) [ sm.set_next Read_count_lo ] ]
              ] )
          ; ( Read_count_lo
            , [ when_
                  i.uart_valid
                  [ expected_words <-- byte_as_16; sm.set_next Read_count_hi ]
              ] )
          ; ( Read_count_hi
            , [ when_
                  i.uart_valid
                  [ expected_words <-- (expected_words.value |: hi_byte)
                  ; sm.set_next Read_data
                  ]
              ] )
          ; ( Read_data
            , [ when_
                  i.uart_valid
                  [ if_
                      (i.uart_byte ==:. end_marker)
                      [ sm.set_next Done ]
                      [ current_word <-- new_word
                      ; if_
                          (byte_index.value ==:. bytes_per_word - 1)
                          [ ram_we <-- vdd
                          ; ram_wdata <-- new_word
                          ; byte_index <--. 0
                          ; current_word <--. 0
                          ; write_addr <-- write_addr.value +:. 1
                          ; words_written <-- words_written.value +:. 1
                          ]
                          [ byte_index <-- byte_index.value +:. 1 ]
                      ]
                  ]
              ] )
          ; Done, []
          ]
      ];
    { O.ram_write_enable = ram_we.value
    ; ram_write_addr = write_addr.value
    ; ram_write_data = ram_wdata.value
    ; word_count = words_written.value
    ; ready = sm.is Done
    }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"loader" create
  ;;
end
