open! Core
open! Hardcaml
open! Signal

let data_width = 16
let addr_bits = 13
let result_width = 64

module Interfaces = Solver_intf.Make_interfaces (struct
    let data_width = data_width
    let addr_bits = addr_bits
    let result_width = result_width
  end)

module I = Interfaces.I
module O = Interfaces.O

module States = struct
  type t =
    | Idle (** Wait for start signal *)
    | Reading (** Check if more inputs, issue RAM read *)
    | Wait_ram (** Wait for RAM latency, decode input *)
    | Ticking (** Process one tick per cycle *)
    | Check_end (** Check for part1 count, advance to next input *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Current read address *)
  let%hw_var read_addr = Variable.reg spec ~width:addr_bits in
  (* Position on dial (0-99), starts at 50 *)
  let%hw_var position = Variable.reg spec ~width:7 in
  (* Direction for current movement: 0=Right, 1=Left *)
  let%hw_var direction = Variable.reg spec ~width:1 in
  (* Remaining ticks in current movement *)
  let%hw_var tick_counter = Variable.reg spec ~width:10 in
  (* Part 1: count of movements ending at position 0 *)
  let%hw_var part1_count = Variable.reg spec ~width:32 in
  (* Part 2: count of ticks where position becomes 0 *)
  let%hw_var part2_count = Variable.reg spec ~width:32 in
  (* Compute next position combinationally (used in Ticking state) *)
  let next_pos_left =
    mux2 (position.value ==:. 0) (of_int_trunc ~width:7 99) (position.value -:. 1)
  in
  let next_pos_right =
    mux2 (position.value ==:. 99) (of_int_trunc ~width:7 0) (position.value +:. 1)
  in
  let next_pos = mux2 (direction.value ==:. 1) next_pos_left next_pos_right in
  compile
    [ sm.switch
        [ ( Idle
          , [ read_addr <--. 0
            ; position <--. 50
            ; part1_count <--. 0
            ; part2_count <--. 0
            ; when_ i.start [ sm.set_next Reading ]
            ] )
        ; ( Reading
          , [ (* Check if we've read all inputs *)
              if_
                (read_addr.value >=: i.input_count)
                [ sm.set_next Done ]
                [ (* Issue RAM read and wait *) sm.set_next Wait_ram ]
            ] )
        ; ( Wait_ram
          , [ (* RAM has 1 cycle latency, data should be valid *)
              (* Decode: bit 10 = direction, bits 14:0 = distance *)
              direction <-- sel_top ~width:1 i.ram_read_data
            ; tick_counter <-- sel_bottom ~width:10 i.ram_read_data
            ; sm.set_next Ticking
            ] )
        ; ( Ticking
          , [ (* Update position register with next_pos (computed above) *)
              position <-- next_pos
            ; (* Check if next position is 0 for part2 *)
              when_ (next_pos ==:. 0) [ incr part2_count ]
            ; (* Decrement tick counter *)
              tick_counter <-- tick_counter.value -:. 1
            ; (* Check if this was the last tick *)
              when_ (tick_counter.value ==:. 1) [ sm.set_next Check_end ]
            ] )
        ; ( Check_end
          , [ (* Check if position is 0 at end of movement for part1 *)
              when_ (position.value ==:. 0) [ incr part1_count ]
            ; (* Advance to next input *)
              incr read_addr
            ; sm.set_next Reading
            ] )
        ; Done, []
        ]
    ];
  { O.ram_read_addr = read_addr.value
  ; part1 = uresize ~width:result_width part1_count.value
  ; part2 = uresize ~width:result_width part2_count.value
  ; done_ = sm.is Done
  ; debug_state = zero 6
  ; debug_insert_pos = zero 16
  ; debug_shift_idx = zero 16
  ; debug_bram_read = zero data_width
  ; debug_cmp_val = zero data_width
  ; debug_write_addr = zero addr_bits
  ; debug_write_data = zero data_width
  ; debug_read_addr = zero addr_bits
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day01_solver" create
;;
