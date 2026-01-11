open! Core
open! Hardcaml
open! Signal

(** ~3700 cols * 4 bits = ~14800 bits *)
let data_width = 14800

let addr_bits = 32
let result_width = 64

module Interfaces = Solver_intf.Make_interfaces (struct
    let data_width = data_width
    let addr_bits = addr_bits
    let result_width = result_width
  end)

module I = Interfaces.I
module O = Interfaces.O

(** Max number of columns in the grid *)
let max_cols = data_width / 4

(** Max rows we support *)
let max_rows = 8

module States = struct
  type t =
    | Idle (** Wait for start, initialize *)
    | Load_row (** Request row from external RAM *)
    | Wait_load (** Wait for RAM latency, store row *)
    | Init_scan (** Initialize block scanning *)
    | Wait_scan (** Wait for scan_col register to update *)
    | Scan_delim (** Check if current column is all-spaces delimiter *)
    | Check_block (** Check if we have a valid block to process *)
    | Get_op (** Extract operator from last row *)
    | P1_row_init (** Initialize Part 1 row parsing *)
    | P1_row_parse (** Parse one cell in current row *)
    | P1_row_done (** End of row, emit number if any *)
    | P1_block_finish (** Wait for block_acc update, then add to part1 *)
    | P2_col_init (** Initialize Part 2 column parsing *)
    | P2_col_parse (** Parse one cell in current column *)
    | P2_col_done (** End of column, emit number if any *)
    | P2_block_finish (** Wait for block_acc update, then add to part2 *)
    | Block_done (** Add block results to totals, continue scanning *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Grid dimensions - packed in input_count: high 16 bits = width, low 16 bits = height *)
  let%hw_var grid_width = Variable.reg spec ~width:16 in
  let%hw_var grid_height = Variable.reg spec ~width:16 in
  (* Row storage - 8 registers of data_width bits each *)
  let row_regs = Array.init max_rows ~f:(fun _ -> Variable.reg spec ~width:data_width) in
  (* Loading state *)
  let%hw_var load_row_idx = Variable.reg spec ~width:4 in
  let%hw_var ram_addr = Variable.reg spec ~width:addr_bits in
  (* Block scanning *)
  let%hw_var scan_col = Variable.reg spec ~width:16 in
  let%hw_var block_left = Variable.reg spec ~width:16 in
  let%hw_var block_right = Variable.reg spec ~width:16 in
  let%hw_var block_op = Variable.reg spec ~width:1 in
  let%hw_var block_acc = Variable.reg spec ~width:result_width in
  let%hw_var first_num = Variable.reg spec ~width:1 in
  (* Part 1 parsing *)
  let%hw_var p1_row = Variable.reg spec ~width:4 in
  let%hw_var p1_col = Variable.reg spec ~width:16 in
  let%hw_var p1_num = Variable.reg spec ~width:result_width in
  let%hw_var p1_in_num = Variable.reg spec ~width:1 in
  (* Part 2 parsing *)
  let%hw_var p2_col = Variable.reg spec ~width:16 in
  let%hw_var p2_row = Variable.reg spec ~width:4 in
  let%hw_var p2_num = Variable.reg spec ~width:result_width in
  let%hw_var p2_in_num = Variable.reg spec ~width:1 in
  (* Results *)
  let%hw_var part1_acc = Variable.reg spec ~width:result_width in
  let%hw_var part2_acc = Variable.reg spec ~width:result_width in
  (* Helper: extract 4-bit cell at column col from a row
     After encoding: cells are concat_msb then uresize, so data is at LOW bits.
     Column 0 is at position (width-1) from the right, column (width-1) is at position 0.
     We pre-extract all possible cells and use runtime mux selection. *)
  let get_cell row col =
    (* Pre-extract all possible 4-bit cells at each position from the RIGHT (LSB) *)
    let all_cells =
      List.init max_cols ~f:(fun i -> select row ~high:((i * 4) + 3) ~low:(i * 4))
    in
    (* Position from right = (grid_width - 1 - col) *)
    let pos_from_right = grid_width.value -:. 1 -: uresize col ~width:16 in
    mux pos_from_right all_cells
  in
  (* Helper: select row register by index *)
  let get_row idx = mux idx (Array.to_list (Array.map row_regs ~f:(fun r -> r.value))) in
  (* Space constant (value 10 in 4-bit encoding) *)
  let space_val = of_int_trunc ~width:4 10 in
  (* Operator constants: '+' = 11, '*' = 12 *)
  let mul_op_val = of_int_trunc ~width:4 12 in
  (* Constant 10 for decimal multiplication *)
  let ten = of_int_trunc ~width:result_width 10 in
  (* Data height is grid_height - 1 (excluding operator row) *)
  let data_height = grid_height.value -:. 1 in
  (* Combinational: check if scan_col is a delimiter (all data cells are spaces)
     only check data rows (0 to data_height-1), not the operator row *)
  let is_col_delimiter =
    let checks =
      List.init max_rows ~f:(fun r ->
        let row_data = row_regs.(r).value in
        let cell = get_cell row_data scan_col.value in
        (* Either this row is >= data_height (operator row or doesn't exist) or cell is space *)
        uresize data_height ~width:8 <=:. r |: (cell ==: space_val))
    in
    List.reduce_exn checks ~f:( &: )
  in
  let p1_current_row = get_row p1_row.value in
  let p1_current_cell = get_cell p1_current_row p1_col.value in
  let p2_current_row = get_row p2_row.value in
  let p2_current_cell = get_cell p2_current_row p2_col.value in
  (* Operator row is the last row (grid_height - 1) *)
  let op_row_idx = grid_height.value -:. 1 in
  let op_row = get_row (sel_bottom op_row_idx ~width:4) in
  let op_cell = get_cell op_row block_left.value in
  (* Accumulate number into block_acc based on operation *)
  let accumulate_num num =
    (* Multiplication produces 128-bit result so truncate to 64 bits *)
    let mul_result = sel_bottom (block_acc.value *: num) ~width:result_width in
    mux2 block_op.value mul_result (block_acc.value +: num)
  in
  compile
    [ sm.switch
        [ (* Idle: Wait for start signal *)
          ( Idle
          , [ grid_width <--. 0
            ; grid_height <--. 0
            ; part1_acc <--. 0
            ; part2_acc <--. 0
            ; load_row_idx <--. 0
            ; ram_addr <--. 0
            ; when_
                i.start
                [ (* Unpack dimensions from input_count *)
                  grid_width <-- sel_top i.input_count ~width:16
                ; grid_height <-- sel_bottom i.input_count ~width:16
                ; ram_addr <--. 0
                ; load_row_idx <--. 0
                ; sm.set_next Load_row
                ]
            ] )
        ; ( Load_row
          , [ (* RAM address is already set, move to wait state *) sm.set_next Wait_load ]
          )
        ; ( Wait_load
          , [ (* Store row data into appropriate register *)
              proc
                (List.init max_rows ~f:(fun r ->
                   when_ (load_row_idx.value ==:. r) [ row_regs.(r) <-- i.ram_read_data ]))
            ; (* Advance to next row *)
              if_
                (uresize load_row_idx.value ~width:16 +:. 1 >=: grid_height.value)
                [ (* All rows loaded, start scanning *) sm.set_next Init_scan ]
                [ incr load_row_idx
                ; ram_addr <-- ram_addr.value +:. 1
                ; sm.set_next Load_row
                ]
            ] )
        ; ( Init_scan
          , [ scan_col <-- grid_width.value -:. 1
            ; block_right <-- grid_width.value
            ; sm.set_next Wait_scan
            ] )
        ; (* wait one cycle for registers to update *)
          Wait_scan, [ sm.set_next Scan_delim ]
        ; (* Check if current column is all-spaces delimiter *)
          ( Scan_delim
          , [ (* Check if scan_col < 0 (we've scanned past the left edge) *)
              if_
                (msb scan_col.value)
                [ (* scan_col is negative, treat column -1 as delimiter *)
                  block_left <--. 0
                ; sm.set_next Check_block
                ]
                [ (* Check if this column is a delimiter (all spaces) *)
                  if_
                    is_col_delimiter
                    [ (* Found delimiter at scan_col *)
                      block_left <-- scan_col.value +:. 1
                    ; sm.set_next Check_block
                    ]
                    [ (* Not a delimiter, continue scanning left *)
                      scan_col <-- scan_col.value -:. 1
                    ]
                ]
            ] )
        ; (* See if we have a valid block to process *)
          ( Check_block
          , [ if_
                (block_left.value <: block_right.value)
                [ (* Valid block exists, get operator *) sm.set_next Get_op ]
                [ (* Empty block, update right boundary and continue scanning *)
                  if_
                    (msb scan_col.value)
                    [ (* scan_col is negative, we're done *) sm.set_next Done ]
                    [ block_right <-- scan_col.value
                    ; scan_col <-- scan_col.value -:. 1
                    ; sm.set_next Wait_scan
                    ]
                ]
            ] )
        ; (* Extract operator from last row at block_left position *)
          ( Get_op
          , [ (* Operator: 12 means multiply, otherwise add *)
              block_op <-- (op_cell ==: mul_op_val)
            ; (* Initialize block accumulator: 1 for multiply, 0 for add *)
              block_acc
              <-- mux2 (op_cell ==: mul_op_val) (one result_width) (zero result_width)
            ; first_num <--. 1
            ; p1_row <--. 0
            ; p1_col <-- block_left.value
            ; p1_num <--. 0
            ; p1_in_num <--. 0
            ; sm.set_next P1_row_init
            ] )
        ; ( P1_row_init
          , [ p1_col <-- block_left.value
            ; p1_num <--. 0
            ; p1_in_num <--. 0
            ; sm.set_next P1_row_parse
            ] )
        ; (* Parse one cell in current row *)
          ( P1_row_parse
          , [ (let cell_is_digit = p1_current_cell <:. 10 in
               if_
                 cell_is_digit
                 [ (* Digit: accumulate into current number *)
                   p1_num
                   <-- sel_bottom (p1_num.value *: ten) ~width:result_width
                       +: uresize p1_current_cell ~width:result_width
                 ; p1_in_num <--. 1
                 ]
                 [ (* Space: if we were in a number, emit it *)
                   when_
                     p1_in_num.value
                     [ if_
                         first_num.value
                         [ block_acc <-- p1_num.value; first_num <--. 0 ]
                         [ block_acc <-- accumulate_num p1_num.value ]
                     ; p1_num <--. 0
                     ; p1_in_num <--. 0
                     ]
                 ])
            ; (* Advance to next column or end of row *)
              if_
                (p1_col.value +:. 1 >=: block_right.value)
                [ sm.set_next P1_row_done ]
                [ p1_col <-- p1_col.value +:. 1 ]
            ] )
        ; (* End of row, emit final number if any *)
          ( P1_row_done
          , [ when_
                p1_in_num.value
                [ if_
                    first_num.value
                    [ block_acc <-- p1_num.value; first_num <--. 0 ]
                    [ block_acc <-- accumulate_num p1_num.value ]
                ]
            ; (* Advance to next row or finish Part 1 *)
              if_
                (uresize p1_row.value ~width:16 +:. 1 >=: data_height)
                [ (* Part 1 done for this block, wait for block_acc update *)
                  sm.set_next P1_block_finish
                ]
                [ incr p1_row; sm.set_next P1_row_init ]
            ] )
        ; (* wait for block_acc register to update, then add to part1 and start Part 2 *)
          ( P1_block_finish
          , [ part1_acc <-- part1_acc.value +: block_acc.value
            ; first_num <--. 1
            ; block_acc <-- mux2 block_op.value (one result_width) (zero result_width)
            ; p2_col <-- block_left.value
            ; p2_row <--. 0
            ; p2_num <--. 0
            ; p2_in_num <--. 0
            ; sm.set_next P2_col_init
            ] )
        ; ( P2_col_init
          , [ p2_row <--. 0; p2_num <--. 0; p2_in_num <--. 0; sm.set_next P2_col_parse ] )
        ; (* parse one cell in current column (top to bottom) *)
          ( P2_col_parse
          , [ (let cell_is_digit = p2_current_cell <:. 10 in
               if_
                 cell_is_digit
                 [ (* Digit: accumulate into current number *)
                   p2_num
                   <-- sel_bottom (p2_num.value *: ten) ~width:result_width
                       +: uresize p2_current_cell ~width:result_width
                 ; p2_in_num <--. 1
                 ; (* Advance to next row *)
                   if_
                     (uresize p2_row.value ~width:16 +:. 1 >=: data_height)
                     [ sm.set_next P2_col_done ]
                     [ incr p2_row ]
                 ]
                 [ (* Space: for vertical parsing, skip leading spaces, stop at space after digits *)
                   if_
                     p2_in_num.value
                     [ (* We were in a number, space means end of this vertical number *)
                       sm.set_next P2_col_done
                     ]
                     [ (* Leading space, skip it *)
                       if_
                         (uresize p2_row.value ~width:16 +:. 1 >=: data_height)
                         [ (* Reached end of column without finding digits *)
                           sm.set_next P2_col_done
                         ]
                         [ incr p2_row ]
                     ]
                 ])
            ] )
        ; (*End of column, emit number if any *)
          ( P2_col_done
          , [ when_
                p2_in_num.value
                [ if_
                    first_num.value
                    [ block_acc <-- p2_num.value; first_num <--. 0 ]
                    [ block_acc <-- accumulate_num p2_num.value ]
                ]
            ; (* Advance to next column or finish Part 2 *)
              if_
                (p2_col.value +:. 1 >=: block_right.value)
                [ (* Part 2 done for this block, wait for block_acc update *)
                  sm.set_next P2_block_finish
                ]
                [ p2_col <-- p2_col.value +:. 1; sm.set_next P2_col_init ]
            ] )
        ; (* wait for block_acc register to update, then add to part2 *)
          ( P2_block_finish
          , [ part2_acc <-- part2_acc.value +: block_acc.value; sm.set_next Block_done ] )
        ; ( Block_done
          , [ (* Update right boundary to current delimiter position *)
              if_
                (msb scan_col.value)
                [ (* scan_col is negative, we're done *) sm.set_next Done ]
                [ block_right <-- scan_col.value
                ; scan_col <-- scan_col.value -:. 1
                ; sm.set_next Wait_scan
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

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day06_solver" create
;;
