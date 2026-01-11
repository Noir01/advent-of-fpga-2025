open! Core
open! Hardcaml
open! Signal

(** Data width for 54-bit numbers (10^16 - 1 fits in 54 bits) *)
let data_width = 54

(** Address bits for RAM addressing (enough for ~2500 entries) *)
let addr_bits = 16

let result_width = 64

(** Internal BRAM address bits for ranges (512 entries = 9 bits) *)
let range_bram_addr_bits = 9

(** Internal BRAM address bits for ingredients (1024 entries = 10 bits) *)
let ingr_bram_addr_bits = 10

module Interfaces = Solver_intf.Make_interfaces (struct
    let data_width = data_width
    let addr_bits = addr_bits
    let result_width = result_width
  end)

module I = Interfaces.I
module O = Interfaces.O

module States = struct
  type t =
    | Idle (** Wait for start *)
    | Load_counts_req (** Request num_ranges from RAM *)
    | Load_counts_wait (** Wait for RAM latency *)
    | Load_counts_num_ranges (** Receive num_ranges, request num_numbers *)
    | Load_counts_num_numbers (** Receive num_numbers *)
    | Load_range_start (** Request range start from external RAM *)
    | Load_range_end (** Request range end, receive start *)
    | Insert_init (** Initialize insertion - receive end, start search *)
    | Insert_find (** Search backward to find insertion point *)
    | Insert_find_wait
    | Insert_shift_read (** Read range to shift *)
    | Insert_shift_write (** Write shifted range *)
    | Insert_write_start (** Write new range start *)
    | Insert_write_end (** Write new range end *)
    | Load_range_next (** Move to next range or transition *)
    | Load_ingredients_req (** Request ingredient from external RAM *)
    | Load_ingredients_write (** Write ingredient to BRAM *)
    | Merge_init (** Initialize merge phase *)
    | Merge_read_start (** Read next range start *)
    | Merge_read_end (** Read next range end *)
    | Merge_process (** Process range - merge or emit *)
    | Merge_emit (** Emit current merged range start *)
    | Merge_emit_end (** Emit current merged range end *)
    | Merge_final (** Emit final merged range start *)
    | Merge_final_end (** Emit final merged range end *)
    | Check_init (** Initialize checking phase *)
    | Check_read_ingr (** Read next ingredient *)
    | Check_wait_ingr (** Wait for ingredient BRAM latency *)
    | Check_read_range_start (** Read range start for comparison *)
    | Check_read_range_end (** Read range end for comparison *)
    | Check_compare (** Compare ingredient with range *)
    | Check_next (** Move to next ingredient *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let open States in
  let sm = State_machine.create (module States) spec in
  (* Counters and indices *)
  let%hw_var num_ranges = Variable.reg spec ~width:16 in
  let%hw_var num_ingredients = Variable.reg spec ~width:16 in
  let%hw_var merged_count = Variable.reg spec ~width:16 in
  let%hw_var ram_addr = Variable.reg spec ~width:addr_bits in
  let%hw_var range_idx = Variable.reg spec ~width:16 in
  let%hw_var ingredient_idx = Variable.reg spec ~width:16 in
  (* Insertion sort state *)

  let%hw_var insert_pos = Variable.reg spec ~width:16 in
  let%hw_var shift_idx = Variable.reg spec ~width:16 in
  let%hw_var new_start = Variable.reg spec ~width:data_width in
  let%hw_var new_end = Variable.reg spec ~width:data_width in
  (* Merge state *)
  let%hw_var merge_start = Variable.reg spec ~width:data_width in
  let%hw_var merge_end = Variable.reg spec ~width:data_width in
  let%hw_var first_range = Variable.reg spec ~width:1 in
  (* Check state *)
  let%hw_var current_ingredient = Variable.reg spec ~width:data_width in
  let%hw_var check_start = Variable.reg spec ~width:data_width in
  let%hw_var ingredient_matched = Variable.reg spec ~width:1 in
  (* Results *)
  let%hw_var part1_acc = Variable.reg spec ~width:result_width in
  let%hw_var part2_acc = Variable.reg spec ~width:result_width in
  (* Range BRAM A - stores sorted ranges as consecutive (start, end) pairs *)
  (* Address 2k = start of range k, Address 2k+1 = end of range k *)
  let%hw_var bram_a_write_enable = Variable.reg spec ~width:1 in
  let%hw_var bram_a_write_addr = Variable.reg spec ~width:range_bram_addr_bits in
  let%hw_var bram_a_write_data = Variable.reg spec ~width:data_width in
  let%hw_var bram_a_read_addr = Variable.reg spec ~width:range_bram_addr_bits in
  let%hw_var bram_a_read_addr_wire = Variable.wire ~default:bram_a_read_addr.value () in
  (* Merged ranges BRAM B - stores merged ranges *)
  let%hw_var bram_b_write_enable = Variable.reg spec ~width:1 in
  let%hw_var bram_b_write_addr = Variable.reg spec ~width:range_bram_addr_bits in
  let%hw_var bram_b_write_data = Variable.reg spec ~width:data_width in
  let%hw_var bram_b_read_addr = Variable.reg spec ~width:range_bram_addr_bits in
  let%hw_var bram_b_read_addr_wire = Variable.wire ~default:bram_b_read_addr.value () in
  (* Ingredient BRAM *)
  let%hw_var ingr_bram_write_enable = Variable.reg spec ~width:1 in
  let%hw_var ingr_bram_write_addr = Variable.reg spec ~width:ingr_bram_addr_bits in
  let%hw_var ingr_bram_write_data = Variable.reg spec ~width:data_width in
  let%hw_var ingr_bram_read_addr = Variable.reg spec ~width:ingr_bram_addr_bits in
  (* Create Range BRAM A *)
  let bram_a_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:512
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = bram_a_write_enable.value
           ; write_address = bram_a_write_addr.value
           ; write_data = bram_a_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_enable = vdd; read_address = bram_a_read_addr_wire.value }
        |]
      ()
  in
  (* Create Merged Ranges BRAM B *)
  let bram_b_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:512
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = bram_b_write_enable.value
           ; write_address = bram_b_write_addr.value
           ; write_data = bram_b_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_enable = vdd; read_address = bram_b_read_addr_wire.value }
        |]
      ()
  in
  (* Create Ingredient BRAM *)
  let ingr_bram_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:1024
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = ingr_bram_write_enable.value
           ; write_address = ingr_bram_write_addr.value
           ; write_data = ingr_bram_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock
           ; read_enable = vdd
           ; read_address = ingr_bram_read_addr.value
           }
        |]
      ()
  in
  (* FSM logic *)
  compile
    [ (* Default: disable BRAM writes *)
      bram_a_write_enable <--. 0
    ; bram_b_write_enable <--. 0
    ; sm.switch
        [ (* === IDLE === *)
          ( Idle
          , [ part1_acc <--. 0
            ; part2_acc <--. 0
            ; ram_addr <--. 0
            ; range_idx <--. 0
            ; ingredient_idx <--. 0
            ; merged_count <--. 0
            ; first_range <--. 1
            ; when_ i.start [ sm.set_next Load_counts_req ]
            ] )
        ; (* === LOAD COUNTS === *)
          (* Request address 0 (num_ranges) *)
          ( Load_counts_req
          , [ ram_addr <--. 0
            ; sm.set_next Load_counts_wait
            ] )
        ; (* Wait one cycle for RAM latency *)
          ( Load_counts_wait
          , [ ram_addr <--. 1 (* Request num_numbers *)
            ; sm.set_next Load_counts_num_ranges
            ] )
        ; (* Receive num_ranges *)
          ( Load_counts_num_ranges
          , [ num_ranges <-- uresize i.ram_read_data ~width:16
            ; ram_addr <--. 2 (* First range start *)
            ; sm.set_next Load_counts_num_numbers
            ] )
        ; (* Receive num_numbers, start loading ranges *)
          ( Load_counts_num_numbers
          , [ num_ingredients <-- uresize i.ram_read_data ~width:16
            ; if_
                (num_ranges.value ==:. 0)
                [ sm.set_next Load_ingredients_req ]
                [ sm.set_next Load_range_start ]
            ] )
        ; (* === LOAD RANGES WITH INLINE INSERTION SORT === *)
          (* Request range start from external RAM *)
          ( Load_range_start
          , [ (* ram_addr is already pointing to start *)
              incr ram_addr (* Point to end for next cycle *)
            ; sm.set_next Load_range_end
            ] )
        ; (* Request range end, receive start *)
          ( Load_range_end
          , [ new_start <-- i.ram_read_data
            ; incr ram_addr (* Point to next range's start *)
            ; sm.set_next Insert_init
            ] )
        ; (* Initialize insertion - start forward search from 0 *)
          ( Insert_init
          , [ new_end <-- i.ram_read_data
            ; if_
                (range_idx.value ==:. 0)
                [ (* First range - just insert at position 0 *)
                  insert_pos <--. 0
                ; sm.set_next Insert_write_start
                ]
                [ (* Start searching forward from 0 *)
                  shift_idx <--. 0 (* Used as scan_idx here *)
                ; bram_a_read_addr <--. 0 (* Request start of range 0 *)
                ; bram_a_read_addr_wire <--. 0
                ; sm.set_next Insert_find
                ]
            ] )
        ; (* Search forward to find insertion point *)
          ( Insert_find
          , [ (* Compare new_start with the start at shift_idx (data arrives this cycle) *)
              if_
                (new_start.value <+ bram_a_read_data.(0))
                [ (* Found insertion point - insert at shift_idx *)
                  insert_pos <-- shift_idx.value
                ; (* Start shifting from range_idx-1 down to insert_pos *)
                  shift_idx <-- range_idx.value -:. 1
                ; (* Request first range to shift (range_idx-1) *)
                  bram_a_read_addr <-- uresize (concat_msb [ (range_idx.value -:. 1); gnd ]) ~width:range_bram_addr_bits
                ; bram_a_read_addr_wire <-- uresize (concat_msb [ (range_idx.value -:. 1); gnd ]) ~width:range_bram_addr_bits
                ; sm.set_next Insert_shift_read
                ]
                [ (* Continue searching forward *)
                  incr shift_idx
                ; if_
                    (shift_idx.value +:. 1 >=+ range_idx.value)
                    [ (* Reached end - insert at range_idx *)
                      insert_pos <-- range_idx.value
                    ; sm.set_next Insert_write_start
                    ]
                    [ (* Request next range start *)
                      bram_a_read_addr
                      <-- uresize (concat_msb [ (shift_idx.value +:. 1); gnd ]) ~width:range_bram_addr_bits
                    ; sm.set_next Insert_find_wait
                    ]
                ]
            ] )
        ; ( Insert_find_wait
          , [ sm.set_next Insert_find ] )
        ; (* Read range to shift - read start first *)
          ( Insert_shift_read
          , [ (* We have start at current read position, request end *)
              bram_a_read_addr <-- bram_a_read_addr.value +:. 1
            ; bram_a_read_addr_wire <-- bram_a_read_addr.value +:. 1
            ; (* Pre-emptively enable write to destination (shift_idx + 1) *)
              bram_a_write_enable <--. 1
            ; bram_a_write_addr
              <-- uresize (concat_msb [ (shift_idx.value +:. 1); gnd ]) ~width:range_bram_addr_bits
            ; bram_a_write_data <-- bram_a_read_data.(0)
            ; sm.set_next Insert_shift_write
            ] )
        ; (* Write shifted range end *)
          ( Insert_shift_write
          , [ (* Write end to position shift_idx + 1 *)
              bram_a_write_enable <--. 1
            ; bram_a_write_addr <-- bram_a_write_addr.value +:. 1
            ; bram_a_write_data <-- bram_a_read_data.(0)
            ; if_
                (shift_idx.value ==: insert_pos.value)
                [ (* Done shifting, write new range *)
                  sm.set_next Insert_write_start
                ]
                [ (* More ranges to shift *)
                  shift_idx <-- shift_idx.value -:. 1
                ; (* Request next range to shift (working backwards) *)
                  bram_a_read_addr <-- uresize (concat_msb [ (shift_idx.value -:. 1); gnd ]) ~width:range_bram_addr_bits
                ; bram_a_read_addr_wire <-- uresize (concat_msb [ (shift_idx.value -:. 1); gnd ]) ~width:range_bram_addr_bits
                ; sm.set_next Insert_shift_read
                ]
            ] )
        ; (* Write new range start at insert_pos *)
          ( Insert_write_start
          , [ bram_a_write_enable <--. 1
            ; bram_a_write_addr <-- uresize (sll insert_pos.value ~by:1) ~width:range_bram_addr_bits
            ; bram_a_write_data <-- new_start.value
            ; sm.set_next Insert_write_end
            ] )
        ; (* Write new range end at insert_pos *)
          ( Insert_write_end
          , [ bram_a_write_enable <--. 1
            ; bram_a_write_addr <-- bram_a_write_addr.value +:. 1
            ; bram_a_write_data <-- new_end.value
            ; sm.set_next Load_range_next
            ] )
        ; (* Done with insertion, move to next range *)
          ( Load_range_next
          , [ incr range_idx
            ; if_
                (range_idx.value +:. 1 >=+ num_ranges.value)
                [ sm.set_next Load_ingredients_req ]
                [ sm.set_next Load_range_start ]
            ] )
        ; (* === LOAD INGREDIENTS === *)
          ( Load_ingredients_req
          , [ sm.set_next Load_ingredients_write
            ; (* Actually we need to read from global ram_addr which continues from ranges *)
              (* But wait, parser format: num_ranges, num_numbers, RANGES... NUMBERS... *)
              (* Ranges take 2 * num_ranges words. *)
              (* Start of numbers is 2 + 2*num_ranges *)
              (* ram_addr currently points to end of last range + 1? *)
              (* Yes, Load_range_end increments it. *)
            ] )
        ; ( Load_ingredients_write
          , [ ingr_bram_write_enable <--. 1
            ; ingr_bram_write_addr <-- uresize ingredient_idx.value ~width:ingr_bram_addr_bits
            ; ingr_bram_write_data <-- i.ram_read_data
            ; incr ingredient_idx
            ; incr ram_addr
            ; if_
                (ingredient_idx.value +:. 1 >=+ num_ingredients.value)
                [ sm.set_next Merge_init ]
                [ (* Need to wait for next read? We are reading continuous here? *)
                  (* External RAM provides data every cycle if addr increments? *)
                  (* If we incr addr in this state, next data available next cycle? *)
                  (* We need a loop with read latency? *)
                  (* Assuming stream: Request T -> Data T+1. *)
                  (* State T: Write Data(T). Req T+1. *)
                  (* Data(T) comes from Req T-1. *)
                  (* This works only if piped correctly. *)
                  sm.set_next Load_ingredients_write
                ]
            ] )
        ; (* === MERGE RANGES === *)
          ( Merge_init
          , [ (* Iterate ranges in BRAM A, merge, write to BRAM B *)
              first_range <--. 1
            ; range_idx <--. 0
            ; merged_count <--. 0
            ; part2_acc <--. 0
            ; bram_a_read_addr <--. 0 (* Request start of range 0 *)
            ; bram_a_read_addr_wire <--. 0
            ; sm.set_next Merge_read_start
            ] )
        ; ( Merge_read_start
          , [ (* Request range end *)
              bram_a_read_addr <-- bram_a_read_addr.value +:. 1
            ; bram_a_read_addr_wire <-- bram_a_read_addr.value +:. 1
            ; new_start <-- bram_a_read_data.(0) (* Capture start data here *)
            ; sm.set_next Merge_read_end
            ] )
        ; ( Merge_read_end
          , [ (* Receive start, will receive end next cycle *)
              if_
                first_range.value
                [ (* First range - initialize merge_start/end *)
                  merge_start <-- new_start.value
                ; first_range <--. 0
                ; sm.set_next Merge_process
                ]
                [ (* Compare with current merged range *)
                  sm.set_next Merge_process
                ]
            ] )
        ; ( Merge_process
          , [ (* Receive end *)
              if_
                first_range.value
                [ (* This shouldn't happen, but handle it *)
                  merge_end <-- bram_a_read_data.(0)
                ; incr range_idx
                ; if_
                    (range_idx.value +:. 1 >=+ num_ranges.value)
                    [ sm.set_next Merge_final ]
                    [ bram_a_read_addr <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                    ; bram_a_read_addr_wire <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                    ; sm.set_next Merge_read_start
                    ]
                ]
                [ (* Check overlap: if new_start <= merge_end + 1, merge *)
                  if_
                    (range_idx.value ==:. 0)
                    [ (* First range processed - just set merge_end *)
                      merge_end <-- bram_a_read_data.(0)
                    ; incr range_idx
                    ; if_
                        (range_idx.value +:. 1 >=+ num_ranges.value)
                        [ sm.set_next Merge_final ]
                        [ bram_a_read_addr
                          <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                        ; bram_a_read_addr_wire <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                        ; sm.set_next Merge_read_start
                        ]
                    ]
                    [ if_
                        (new_start.value <=+ merge_end.value +:. 1)
                        [ (* Overlapping or adjacent - extend *)
                          merge_end <-- mux2 (bram_a_read_data.(0) >+ merge_end.value) bram_a_read_data.(0) merge_end.value
                        ; incr range_idx
                        ; if_
                            (range_idx.value +:. 1 >=+ num_ranges.value)
                            [ sm.set_next Merge_final ]
                            [ bram_a_read_addr
                              <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                            ; bram_a_read_addr_wire <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                            ; sm.set_next Merge_read_start
                            ]
                        ]
                        [ (* Gap - emit current merged range *)
                          sm.set_next Merge_emit
                        ; new_end <-- bram_a_read_data.(0)
                        ]
                    ]
                ]
            ] )
        ; ( Merge_emit
          , [ (* Write merged range start to BRAM B *)
              bram_b_write_enable <--. 1
            ; bram_b_write_addr <-- uresize (sll merged_count.value ~by:1) ~width:range_bram_addr_bits
            ; bram_b_write_data <-- merge_start.value
            ; (* Accumulate Part 2 *)
              part2_acc <-- part2_acc.value +: uresize (mux2 (merge_end.value >=+ merge_start.value) (merge_end.value -: merge_start.value +:. 1) (zero data_width)) ~width:result_width
            ; (* Save the new range values for next iteration *)
              (* new_start is already captured in Merge_read_start *)
              sm.set_next Merge_emit_end
            ] )
        ; ( Merge_emit_end
          , [ (* Write merged range end to BRAM B *)
              bram_b_write_enable <--. 1
            ; bram_b_write_addr <-- bram_b_write_addr.value +:. 1
            ; bram_b_write_data <-- merge_end.value (* Write accumulated end *)
            ; (* Start new merged range *)
              merge_start <-- new_start.value
            ; merge_end <-- new_end.value
            ; incr merged_count
            ; incr range_idx
            ; if_
                (range_idx.value +:. 1 >=+ num_ranges.value)
                [ sm.set_next Merge_final ]
                [ bram_a_read_addr
                  <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                ; bram_a_read_addr_wire <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                ; sm.set_next Merge_read_start
                ]
            ] )
        ; ( Merge_final
          , [ (* Write final merged range start to BRAM B *)
              bram_b_write_enable <--. 1
            ; bram_b_write_addr <-- uresize (sll merged_count.value ~by:1) ~width:range_bram_addr_bits
            ; bram_b_write_data <-- merge_start.value
            ; (* Accumulate Part 2 *)
              part2_acc <-- part2_acc.value +: uresize (mux2 (merge_end.value >=+ merge_start.value) (merge_end.value -: merge_start.value +:. 1) (zero data_width)) ~width:result_width
            ; sm.set_next Merge_final_end
            ] )
        ; ( Merge_final_end
          , [ (* Write final merged range end to BRAM B *)
              bram_b_write_enable <--. 1
            ; bram_b_write_addr <-- bram_b_write_addr.value +:. 1
            ; bram_b_write_data <-- merge_end.value
            ; incr merged_count
            ; sm.set_next Check_init
            ] )
        ; (* === CHECK INGREDIENTS === *)
          ( Check_init
          , [ ingredient_idx <--. 0
            ; part1_acc <--. 0
            ; ingr_bram_read_addr <--. 0
            ; sm.set_next Check_read_ingr
            ] )
        ; ( Check_read_ingr
          , [ (* Read ingredient from BRAM *)
              sm.set_next Check_wait_ingr
            ] )
        ; ( Check_wait_ingr
          , [ (* Wait for BRAM latency *)
              (* In this cycle, read_data is valid *)
              current_ingredient <-- ingr_bram_read_data.(0)
            ; range_idx <--. 0
            ; ingredient_matched <--. 0
            ; bram_b_read_addr <--. 0 (* Request 1st range start *)
            ; bram_b_read_addr_wire <--. 0
            ; sm.set_next Check_read_range_start
            ] )
        ; ( Check_read_range_start
          , [ (* Request range end *)
              bram_b_read_addr <-- bram_b_read_addr.value +:. 1
            ; bram_b_read_addr_wire <-- bram_b_read_addr.value +:. 1
            ; check_start <-- bram_b_read_data.(0) (* Capture start data here *)
            ; sm.set_next Check_read_range_end
            ] )
        ; ( Check_read_range_end
          , [ (* Receive start, wait for end *)
              sm.set_next Check_compare
            ] )
        ; ( Check_compare
          , [ (* Receive end *)
              (* Check if ingredient in [check_start, check_end] *)
              (* check_end is bram_b_read_data.(0) *)
              if_
                ((current_ingredient.value >=+ check_start.value)
                &: (current_ingredient.value <=+ bram_b_read_data.(0)))
                [ (* Matched! *)
                  ingredient_matched <--. 1
                ; sm.set_next Check_next
                ]
                [ (* Not matched in this range, try next *)
                  incr range_idx
                ; if_
                    (range_idx.value +:. 1 >=+ merged_count.value)
                    [ (* Checked all ranges, move to next ingredient *)
                      sm.set_next Check_next
                    ]
                    [ (* Verify next range *)
                      bram_b_read_addr <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                    ; bram_b_read_addr_wire <-- uresize (sll (range_idx.value +:. 1) ~by:1) ~width:range_bram_addr_bits
                    ; sm.set_next Check_read_range_start
                    ]
                ]
            ] )
        ; ( Check_next
          , [ (* If not matched, increment Part 1 *)
              if_ (ingredient_matched.value) [ incr part1_acc ] []
            ; incr ingredient_idx
            ; if_
                (ingredient_idx.value +:. 1 >=+ num_ingredients.value)
                [ sm.set_next Done ]
                [ ingr_bram_read_addr <-- uresize (ingredient_idx.value +:. 1) ~width:ingr_bram_addr_bits
                ; sm.set_next Check_read_ingr
                ]
            ] )
        ; ( Done
          , [ (* Done *) ] )
        ]
    ];
  { O.ram_read_addr = ram_addr.value
  ; part1 = part1_acc.value
  ; part2 = part2_acc.value
  ; done_ = sm.is Done
  ; debug_state = zero 6
  ; debug_insert_pos = zero 16
  ; debug_shift_idx = zero 16
  ; debug_bram_read = zero 54
  ; debug_cmp_val = zero 54
  ; debug_write_addr = zero addr_bits
  ; debug_write_data = zero 54
  ; debug_read_addr = zero addr_bits
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day05_solver" create
;;
