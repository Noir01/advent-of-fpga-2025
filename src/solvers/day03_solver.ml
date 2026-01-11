open! Core
open! Hardcaml
open! Signal

let data_width = 400 (* 100 digits × 4 bits each *)
let addr_bits = 8 (* up to 256 lines *)
let result_width = 64
let num_batteries_p1 = 2
let num_batteries_p2 = 12
let num_digits = 100

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
    | Load_line (** Issue RAM read for current line *)
    | Wait_line (** Wait for RAM latency, latch line data *)
    | Init_batteries (** Initialize batteries with rightmost digits *)
    | Process_digit (** Process one digit, parallel battery update *)
    | Check_digit_done (** Check if more digits to process *)
    | Joltage_p1 (** Compute part 1 joltage and accumulate *)
    | Joltage_p2_init (** Initialize part 2 joltage computation *)
    | Joltage_p2_loop (** Iterative joltage computation *)
    | Accumulate (** Add joltage to part 2 accumulator *)
    | Next_line (** Check for more lines *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var line_idx = Variable.reg spec ~width:addr_bits in
  let%hw_var digit_idx = Variable.reg spec ~width:7 in

  let%hw_var line_data = Variable.reg spec ~width:data_width in
  (* Part 1: 2 registers *)
  let batteries_p1 = Array.init num_batteries_p1 ~f:(fun _ -> Variable.reg spec ~width:4) in
  (* Part 2: 12 registers *)
  let batteries_p2 = Array.init num_batteries_p2 ~f:(fun _ -> Variable.reg spec ~width:4) in

  let%hw_var joltage_idx = Variable.reg spec ~width:4 in
  let%hw_var joltage = Variable.reg spec ~width:48 in

  let%hw_var part1_acc = Variable.reg spec ~width:result_width in
  let%hw_var part2_acc = Variable.reg spec ~width:result_width in
  (* extract digit at position from line_data
     digit[0] is at MSB (bits 399:396), digit[99] is at LSB (bits 3:0)
     pos_from_right=0 means bits 3:0, pos_from_right=99 means bits 399:396 *)
  let get_digit_from_signal data pos =
    let pos_from_right = of_int_trunc ~width:7 99 -: pos in
    mux pos_from_right (List.init 100 ~f:(fun i -> select data ~high:((i * 4) + 3) ~low:(i * 4)))
  in
  let get_digit pos = get_digit_from_signal line_data.value pos in

  let current_digit = get_digit digit_idx.value in

  let compute_new_batteries ~batteries_vals ~incoming =
    let num_batteries = Array.length batteries_vals in
    (* inserted[k] = whether to insert/swap at position k *)
    let inserted =
      Array.init num_batteries ~f:(fun k ->
        if k = 0
        then incoming >=: batteries_vals.(0)
        else
          (* inserted[k] = inserted[k-1] AND batteries[k-1] >= batteries[k] *)
          let initial_insert = incoming >=: batteries_vals.(0) in
          let ripple_ok =
            Array.foldi batteries_vals ~init:vdd ~f:(fun m acc bat ->
              if m > 0 && m <= k
              then acc &: (batteries_vals.(m - 1) >=: bat)
              else acc)
          in
          initial_insert &: ripple_ok)
    in

    Array.init num_batteries ~f:(fun k ->
      if k = 0
      then mux2 inserted.(0) incoming batteries_vals.(0)
      else mux2 inserted.(k) batteries_vals.(k - 1) batteries_vals.(k))
  in

  let batteries_p1_vals = Array.map batteries_p1 ~f:(fun b -> b.value) in
  let new_batteries_p1 =
    compute_new_batteries ~batteries_vals:batteries_p1_vals ~incoming:current_digit
  in

  let batteries_p2_vals = Array.map batteries_p2 ~f:(fun b -> b.value) in
  let new_batteries_p2 =
    compute_new_batteries ~batteries_vals:batteries_p2_vals ~incoming:current_digit
  in
  (* Part 1 joltage: batteries_p1[0] * 10 + batteries_p1[1] *)
  let part1_joltage =
    let b0 = uresize batteries_p1.(0).value ~width:8 in
    let b1 = uresize batteries_p1.(1).value ~width:8 in
    let b0_times_10 = sll b0 ~by:3 +: sll b0 ~by:1 in
    b0_times_10 +: b1
  in
  (* Part 2 iterative: joltage = joltage * 10 + batteries_p2[joltage_idx] *)
  let current_battery_for_joltage =
    mux joltage_idx.value (Array.to_list (Array.map batteries_p2 ~f:(fun b -> b.value)))
  in
  let joltage_times_10 =
    let j = joltage.value in
    sll j ~by:3 +: sll j ~by:1
  in
  let next_joltage = joltage_times_10 +: uresize current_battery_for_joltage ~width:48 in
  (* Part 1: start at (100 - 2 - 1) = 97
     Part 2: start at (100 - 12 - 1) = 87
     start at the maximum (97) and conditionally update each battery set *)
  let start_digit_idx_p1 = num_digits - num_batteries_p1 - 1 in
  (* 97 *)
  let start_digit_idx_p2 = num_digits - num_batteries_p2 - 1 in
  (* 87 *)
  compile
    [ sm.switch
        [ ( Idle
          , [ line_idx <--. 0
            ; part1_acc <--. 0
            ; part2_acc <--. 0
            ; when_ i.start [ sm.set_next Load_line ]
            ] )
        ; ( Load_line
          , [ sm.set_next Wait_line ] )
        ; ( Wait_line
          , [ line_data <-- i.ram_read_data
            ; sm.set_next Init_batteries
            ] )
        ; ( Init_batteries
          , [ (* initialize Part 1 batteries with digits 98-99 (rightmost 2) *)
              proc
                (Array.to_list
                   (Array.mapi batteries_p1 ~f:(fun j bat ->
                      let digit_pos = num_digits - num_batteries_p1 + j in
                      (* 98, 99 *)
                      let pos_from_right = 99 - digit_pos in
                      let high = (pos_from_right * 4) + 3 in
                      let low = pos_from_right * 4 in
                      bat <-- select i.ram_read_data ~high ~low)))
            ; (* initialize Part 2 batteries with digits 88-99 (rightmost 12) *)
              proc
                (Array.to_list
                   (Array.mapi batteries_p2 ~f:(fun j bat ->
                      let digit_pos = num_digits - num_batteries_p2 + j in
                      (* 88, 89, ..., 99 *)
                      let pos_from_right = 99 - digit_pos in
                      let high = (pos_from_right * 4) + 3 in
                      let low = pos_from_right * 4 in
                      bat <-- select i.ram_read_data ~high ~low)))
            ; digit_idx <--. start_digit_idx_p1
            ; sm.set_next Process_digit
            ] )
        ; ( Process_digit
          , [ (* always update Part 1 batteries (we start at digit 97) *)
              proc
                (Array.to_list
                   (Array.mapi batteries_p1 ~f:(fun j bat -> bat <-- new_batteries_p1.(j))))
            ; (* update Part 2 batteries only when in their range (digits 0-87) *)
              when_
                (digit_idx.value <=:. start_digit_idx_p2)
                [ proc
                    (Array.to_list
                       (Array.mapi batteries_p2 ~f:(fun j bat -> bat <-- new_batteries_p2.(j))))
                ]
            ; sm.set_next Check_digit_done
            ] )
        ; ( Check_digit_done
          , [ if_
                (digit_idx.value >:. 0)
                [ digit_idx <-- digit_idx.value -:. 1; sm.set_next Process_digit ]
                [ sm.set_next Joltage_p1 ]
            ] )
        ; ( Joltage_p1
          , [ part1_acc <-- part1_acc.value +: uresize part1_joltage ~width:result_width
            ; sm.set_next Joltage_p2_init
            ] )
        ; ( Joltage_p2_init
          , [ joltage <--. 0; joltage_idx <--. 0; sm.set_next Joltage_p2_loop ] )
        ; ( Joltage_p2_loop
          , [ joltage <-- next_joltage
            ; if_
                (joltage_idx.value ==:. (num_batteries_p2 - 1))
                [ sm.set_next Accumulate ]
                [ joltage_idx <-- joltage_idx.value +:. 1 ]
            ] )
        ; ( Accumulate
          , [ part2_acc <-- part2_acc.value +: uresize joltage.value ~width:result_width
            ; sm.set_next Next_line
            ] )
        ; ( Next_line
          , [ incr line_idx
            ; if_
                (line_idx.value +:. 1 >=: i.input_count)
                [ sm.set_next Done ]
                [ sm.set_next Load_line ]
            ] )
        ; (Done, [])
        ]
    ];
  { O.ram_read_addr = uresize line_idx.value ~width:addr_bits
  ; part1 = part1_acc.value
  ; part2 = part2_acc.value
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
  Scoped.hierarchical ~scope ~name:"day03_solver" create
;;
