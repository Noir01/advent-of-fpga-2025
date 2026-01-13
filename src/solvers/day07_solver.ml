open! Core
open! Hardcaml
open! Signal

(** 141 cols = 282 bits *)
let data_width = 282

let addr_bits = 32
let result_width = 64

module Interfaces = Solver_intf.Make_interfaces (struct
    let data_width = data_width
    let addr_bits = addr_bits
    let result_width = result_width
  end)

module I = Interfaces.I
module O = Interfaces.O

let max_cols = data_width / 2

(** Width of timeline counts (64 bits to match result_width) *)
let count_width = 64

module States = struct
  type t =
    | Idle (** Wait for start, initialize *)
    | Init (** Zero out timelines BRAM *)
    | Set_center (** Set timelines[center] = 1 *)
    | Load_row (** Request grid row from external RAM *)
    | Wait_row (** Wait for row data *)
    | Process_init (** Setup for processing this row *)
    | Read_timeline (** Request read of timelines[x] *)
    | Wait_read (** Wait for BRAM read *)
    | Check_process (** Check if split needed *)
    | Update_x (** Write timelines[x] = 0, request read of x-1 *)
    | Wait_left (** Wait for left read *)
    | Write_left (** Write updated left value, request read of x+1 *)
    | Wait_right (** Wait for right read *)
    | Write_right (** Write updated right value *)
    | Next_col (** Move to next column *)
    | Next_row (** Move to next row *)
    | Sum_init (** Initialize summation *)
    | Sum_read (** Request read for summation *)
    | Sum_wait (** Wait for sum read *)
    | Sum_next (** Move to next address in sum *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var grid_width = Variable.reg spec ~width:16 in
  let%hw_var grid_height = Variable.reg spec ~width:16 in
  let%hw_var center = Variable.reg spec ~width:16 in
  let%hw_var row_y = Variable.reg spec ~width:16 in
  let%hw_var col_x = Variable.reg spec ~width:16 in
  (* Address for initialization *)
  let%hw_var init_addr = Variable.reg spec ~width:16 in
  (* Address for summation *)
  let%hw_var sum_addr = Variable.reg spec ~width:16 in
  let%hw_var current_row = Variable.reg spec ~width:data_width in
  let%hw_var current_count = Variable.reg spec ~width:count_width in
  let%hw_var part1_acc = Variable.reg spec ~width:result_width in
  let%hw_var part2_acc = Variable.reg spec ~width:result_width in
  (* BRAM control *)
  let%hw_var bram_write_enable = Variable.reg spec ~width:1 in
  let%hw_var bram_write_addr = Variable.reg spec ~width:16 in
  let%hw_var bram_write_data = Variable.reg spec ~width:count_width in
  let%hw_var bram_read_addr = Variable.reg spec ~width:16 in
  (* External RAM read address *)
  let%hw_var ram_addr = Variable.reg spec ~width:addr_bits in
  (* Create timelines BRAM: 256 entries of 64 bits *)
  let bram_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:max_cols
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = bram_write_enable.value
           ; write_address = uresize bram_write_addr.value ~width:8
           ; write_data = bram_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock
           ; read_enable = vdd
           ; read_address = uresize bram_read_addr.value ~width:8
           }
        |]
      ()
  in
  (* Extract 2-bit cell at position col from row.
     After encoding cells are concat_msb then uresize, so data is at LOW bits
     Column 0 is at position (grid_width-1) from the right, column (grid_width-1) is at position 0
     pre-extract all possible cells and use runtime mux selection *)
  let get_cell row col =
    (* Pre-extract all possible 2-bit cells at each position from the RIGHT (LSB) *)
    let all_cells =
      List.init max_cols ~f:(fun i -> select row ~high:((i * 2) + 1) ~low:(i * 2))
    in
    (* Position from right = (grid_width - 1 - col) *)
    let pos_from_right = grid_width.value -:. 1 -: uresize col ~width:16 in
    mux pos_from_right all_cells
  in
  (* Check if cell is a splitter (value 1 = '^') *)
  let is_splitter = get_cell current_row.value col_x.value ==:. 1 in
  (* Number of processed rows = (grid_height - 2) / 2
     We skip first 2 rows, then take every 2nd row *)
  let num_processed_rows = srl (grid_height.value -:. 2) ~by:1 in
  (* Compute grid row index from processed row: grid_row = row_y * 2 + 2 *)
  let grid_row_idx = sll row_y.value ~by:1 +:. 2 in
  compile
    [ (* disable BRAM writes *)
      bram_write_enable <--. 0
    ; sm.switch
        [ ( Idle
          , [ when_
                i.start
                [ grid_width <-- sel_top i.input_count ~width:16
                ; grid_height <-- sel_bottom i.input_count ~width:16
                ; center <-- srl (sel_top i.input_count ~width:16) ~by:1
                ; row_y <--. 0
                ; col_x <--. 0
                ; init_addr <--. 0
                ; sum_addr <--. 0
                ; part1_acc <--. 0
                ; part2_acc <--. 0
                ; current_count <--. 0
                ; sm.set_next Init
                ]
            ] )
        ; (* Zero out timelines BRAM *)
          ( Init
          , [ bram_write_enable <--. 1
            ; bram_write_addr <-- init_addr.value
            ; bram_write_data <--. 0
            ; if_
                (init_addr.value +:. 1 >=: grid_width.value)
                [ (* Done initializing, set center *)
                  init_addr <--. 0
                ; sm.set_next Set_center
                ]
                [ incr init_addr ]
            ] )
        ; (* Set timelines[center] = 1 *)
          ( Set_center
          , [ bram_write_enable <--. 1
            ; bram_write_addr <-- center.value
            ; bram_write_data <--. 1
            ; row_y <--. 0
            ; sm.set_next Load_row
            ] )
        ; (* Request grid row from external RAM *)
          ( Load_row
          , [ ram_addr <-- uresize grid_row_idx ~width:addr_bits; sm.set_next Wait_row ] )
        ; (* 1 cycle latency *)
          Wait_row, [ current_row <-- i.ram_read_data; sm.set_next Process_init ]
        ; (* Setup for processing this row: x = center - row_y *)
          ( Process_init
          , [ col_x <-- center.value -: row_y.value; sm.set_next Read_timeline ] )
        ; (* Request read of timelines[x] *)
          Read_timeline, [ bram_read_addr <-- col_x.value; sm.set_next Wait_read ]
        ; (* 1 cycle latency
        data arrives at end of this cycle *)
          Wait_read, [ sm.set_next Check_process ]
        ; (* split needed? count > 0 AND cell is splitter
             now bram_read_data has the valid value from the read *)
          ( Check_process
          , [ (* Capture the count now that it's valid *)
              current_count <-- bram_read_data.(0)
            ; if_
                (bram_read_data.(0) >:. 0 &: is_splitter)
                [ (* Split! Increment part1, then update timelines *)
                  part1_acc <-- part1_acc.value +:. 1
                ; sm.set_next Update_x
                ]
                [ (* No split, move to next column *) sm.set_next Next_col ]
            ] )
        ; (* Write timelines[x] = 0 *)
          ( Update_x
          , [ bram_write_enable <--. 1
            ; bram_write_addr <-- col_x.value
            ; bram_write_data <--. 0
            ; (* Request read of x-1 *)
              bram_read_addr <-- col_x.value -:. 1
            ; sm.set_next Wait_left
            ] )
        ; (* Wait for left read *)
          Wait_left, [ sm.set_next Write_left ]
        ; (* Write timelines[x-1] += count *)
          ( Write_left
          , [ bram_write_enable <--. 1
            ; bram_write_addr <-- col_x.value -:. 1
            ; bram_write_data <-- bram_read_data.(0) +: current_count.value
            ; (* Request read of x+1 *)
              bram_read_addr <-- col_x.value +:. 1
            ; sm.set_next Wait_right
            ] )
        ; (* Wait for right read *)
          Wait_right, [ sm.set_next Write_right ]
        ; (* Write timelines[x+1] += count *)
          ( Write_right
          , [ bram_write_enable <--. 1
            ; bram_write_addr <-- col_x.value +:. 1
            ; bram_write_data <-- bram_read_data.(0) +: current_count.value
            ; sm.set_next Next_col
            ] )
        ; (* Move to next column: x += 2, or go to next row if done *)
          ( Next_col
          , [ (* Check if next column would be out of bounds: col_x + 2 > center + row_y *)
              if_
                (col_x.value +:. 2 >: center.value +: row_y.value)
                [ (* Done with this row, go to next *) sm.set_next Next_row ]
                [ (* More columns to process *)
                  col_x <-- col_x.value +:. 2
                ; sm.set_next Read_timeline
                ]
            ] )
        ; ( Next_row
          , [ row_y <-- row_y.value +:. 1
            ; if_
                (row_y.value +:. 1 >=: num_processed_rows)
                [ (* All rows done, sum timelines *)
                  sum_addr <--. 0
                ; sm.set_next Sum_init
                ]
                [ (* More rows to process *) sm.set_next Load_row ]
            ] )
        ; Sum_init, [ bram_read_addr <-- sum_addr.value; sm.set_next Sum_read ]
        ; (* Request read for summation - wait cycle *)
          Sum_read, [ sm.set_next Sum_wait ]
        ; (* Wait for sum read, add to part2 *)
          ( Sum_wait
          , [ part2_acc <-- part2_acc.value +: bram_read_data.(0); sm.set_next Sum_next ]
          )
        ; ( Sum_next
          , [ (* Check if we've summed all addresses: sum_addr + 1 >= grid_width *)
              if_
                (sum_addr.value +:. 1 >=: grid_width.value)
                [ (* done summing! *) sm.set_next Done ]
                [ (* More to sum - request next address before incrementing *)
                  bram_read_addr <-- sum_addr.value +:. 1
                ; sum_addr <-- sum_addr.value +:. 1
                ; sm.set_next Sum_read
                ]
            ] )
        ; Done, []
        ]
    ];
  { O.ram_read_addr = ram_addr.value
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

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"day07_solver" create i
;;
