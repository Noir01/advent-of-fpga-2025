open! Core
open! Hardcaml
open! Signal

(** Max grid width with border (135 + 2 = 137 for real input) *)
let data_width = 137

(** Address bits for row addressing (packed width:height in input_count) *)
let addr_bits = 16

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
    | Idle (** Wait for start, initialize *)
    | Load_row (** Load rows from external RAM to BRAM A *)
    | Prime (** Prime 3-row buffer, handle BRAM latency *)
    | Scan (** Process cells column by column *)
    | Write_row (** Write processed row to output BRAM *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Grid dimensions (with border) - packed in input_count: high 8 bits = width, low 8 bits = height *)
  let%hw_var grid_width = Variable.reg spec ~width:8 in
  let%hw_var grid_height = Variable.reg spec ~width:8 in

  let%hw_var row_idx = Variable.reg spec ~width:8 in
  let%hw_var col_idx = Variable.reg spec ~width:8 in
  let%hw_var load_row_idx = Variable.reg spec ~width:8 in
  (* 3-row sliding buffer *)
  let%hw_var row_top = Variable.reg spec ~width:data_width in
  let%hw_var row_mid = Variable.reg spec ~width:data_width in
  let%hw_var row_bot = Variable.reg spec ~width:data_width in

  let%hw_var output_row = Variable.reg spec ~width:data_width in
  (* counter for initial 3-row loading *)
  let%hw_var prime_count = Variable.reg spec ~width:2 in
  (* Mode and tracking *)
  let%hw_var part1_mode = Variable.reg spec ~width:1 in
  let%hw_var changes_this_gen = Variable.reg spec ~width:1 in
  (* Double buffer control: 0 = read A/write B, 1 = read B/write A *)
  let%hw_var active_buffer = Variable.reg spec ~width:1 in
  (* Shift mode flag for prime state *)
  let%hw_var is_shifting = Variable.reg spec ~width:1 in

  let%hw_var part1_acc = Variable.reg spec ~width:result_width in
  let%hw_var part2_acc = Variable.reg spec ~width:result_width in

  let%hw_var bram_a_write_enable = Variable.reg spec ~width:1 in
  let%hw_var bram_a_write_addr = Variable.reg spec ~width:8 in
  let%hw_var bram_a_write_data = Variable.reg spec ~width:data_width in
  let%hw_var bram_a_read_addr = Variable.reg spec ~width:8 in

  let%hw_var bram_b_write_enable = Variable.reg spec ~width:1 in
  let%hw_var bram_b_write_addr = Variable.reg spec ~width:8 in
  let%hw_var bram_b_write_data = Variable.reg spec ~width:data_width in
  let%hw_var bram_b_read_addr = Variable.reg spec ~width:8 in
  (* Create BRAM A *)
  let bram_a_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:256
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = bram_a_write_enable.value
           ; write_address = bram_a_write_addr.value
           ; write_data = bram_a_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_enable = vdd; read_address = bram_a_read_addr.value }
        |]
      ()
  in
  (* Create BRAM B *)
  let bram_b_read_data =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:256
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = bram_b_write_enable.value
           ; write_address = bram_b_write_addr.value
           ; write_data = bram_b_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_enable = vdd; read_address = bram_b_read_addr.value }
        |]
      ()
  in
  (* Select read BRAM based on active_buffer *)
  let bram_read_data =
    mux2 active_buffer.value bram_b_read_data.(0) bram_a_read_data.(0)
  in
  (* extract bit at column c from row *)
  let get_bit row col =
    let bit_idx = grid_width.value -:. 1 -: col in
    mux bit_idx (bits_lsb row)
  in
  (* set bit at column c in row to value *)
  let set_bit row col value =
    let bit_idx = grid_width.value -:. 1 -: col in
    let bit_positions =
      List.init data_width ~f:(fun i ->
        (* create mask by shifting 1 to position i *)
        let one = one data_width in
        let mask = sll one ~by:i in
        mux2 value (row |: mask) (row &: ~:mask))
    in
    mux bit_idx bit_positions
  in

  let col = col_idx.value in
  let col_left = col -:. 1 in
  let col_right = col +:. 1 in
  let center = get_bit row_mid.value col in
  (* Part 1: all neighbors from original buffers *)
  let get_neighbors_original () =
    [ get_bit row_top.value col_left (* NW *)
    ; get_bit row_top.value col (* N *)
    ; get_bit row_top.value col_right (* NE *)
    ; get_bit row_mid.value col_left (* W *)
    ; get_bit row_mid.value col_right (* E *)
    ; get_bit row_bot.value col_left (* SW *)
    ; get_bit row_bot.value col (* S *)
    ; get_bit row_bot.value col_right (* SE *)
    ]
  in
  (* Part 2: in-place modified neighbors
     - Above row already processed this generation
     - Left cell uses output_row (already processed this row)
     - Right and below use original values *)
  let get_neighbors_inplace () =
    [ get_bit row_top.value col_left (* NW - processed *)
    ; get_bit row_top.value col (* N - processed *)
    ; get_bit row_top.value col_right (* NE - processed *)
    ; get_bit output_row.value col_left (* W - use output_row *)
    ; get_bit row_mid.value col_right (* E - original *)
    ; get_bit row_bot.value col_left (* SW - original *)
    ; get_bit row_bot.value col (* S - original *)
    ; get_bit row_bot.value col_right (* SE - original *)
    ]
  in
  (* Part 1 *)
  let neighbor_count_original =
    let neighbors = get_neighbors_original () in
    List.fold neighbors ~init:(zero 4) ~f:(fun acc n -> acc +: uresize n ~width:4)
  in
  (* Part 2 *)
  let neighbor_count_inplace =
    let neighbors = get_neighbors_inplace () in
    List.fold neighbors ~init:(zero 4) ~f:(fun acc n -> acc +: uresize n ~width:4)
  in
  (* Select neighbor count based on mode *)
  let neighbor_count =
    mux2 part1_mode.value neighbor_count_original neighbor_count_inplace
  in
  (* Cell is weak if center=1 AND neighbor_count < 4 *)
  let is_weak = center &: (neighbor_count <:. 4) in
  (* Cell survives if center=1 AND neighbor_count >= 4 *)
  let survives = center &: (neighbor_count >=:. 4) in
  (* FSM logic *)
  compile
    [ (* Default: disable BRAM writes *)
      bram_a_write_enable <--. 0
    ; bram_b_write_enable <--. 0
    ; sm.switch
        [
          ( Idle
          , [
              load_row_idx <--. 0
            ; row_idx <--. 1 (* Start at row 1, first data row *)
            ; col_idx <--. 1 (* Start at col 1, first data col *)
            ; prime_count <--. 0
            ; part1_mode <--. 1 (* Start with Part 1 *)
            ; changes_this_gen <--. 0
            ; active_buffer <--. 0 (* Read from A, write to B *)
            ; is_shifting <--. 0
            ; part1_acc <--. 0
            ; part2_acc <--. 0
            ; output_row <--. 0
            ; row_top <--. 0
            ; row_mid <--. 0
            ; row_bot <--. 0
            ; when_
                i.start
                [ (* Unpack dimensions from input_count *)
                  grid_width <-- sel_top i.input_count ~width:8
                ; grid_height <-- sel_bottom i.input_count ~width:8
                ; sm.set_next Load_row
                ]
            ] )
        ;
          (* Load external RAM data into BRAM A *)
          ( Load_row
          , [ bram_a_write_enable <--. 1
            ; bram_a_write_addr <-- load_row_idx.value
            ; bram_a_write_data <-- i.ram_read_data
            ; if_
                (load_row_idx.value +:. 1 >=: grid_height.value)
                [ (* All rows loaded, start priming *)
                  load_row_idx <--. 0
                ; row_idx <--. 1
                ; prime_count <--. 0
                ; is_shifting <--. 0
                ; bram_a_read_addr <--. 0 (* Request row 0 *)
                ; bram_b_read_addr <--. 0
                ; sm.set_next Prime
                ]
                [ incr load_row_idx
                ; sm.set_next Load_row
                ]
            ] )
        ;
          (* Prime the 3-row buffer because of BRAM 1-cycle latency *)
          ( Prime
          , [ if_
                is_shifting.value
                [ (* Shift mode: Request sent in Write_row. Wait 1 cycle for data. *)
                  if_
                    (prime_count.value ==:. 0)
                    [ (* Cycle 0 *)
                      prime_count <--. 1 ]
                    [ (* Cycle 1 *)
                      row_bot <-- bram_read_data
                    ; col_idx <--. 1
                    ; output_row <-- row_mid.value
                    ; prime_count <--. 0
                    ; sm.set_next Scan
                    ]
                ]
                [ (* Initial mode: Load 3 rows (0, 1, 2) *)
                  if_
                    (prime_count.value ==:. 0)
                    [ (* Cycle 0: Addr 0 requested at transition. Request Row 1. *)
                      bram_a_read_addr <-- row_idx.value
                    ; bram_b_read_addr <-- row_idx.value
                    ; prime_count <--. 1
                    ]
                    [ if_
                        (prime_count.value ==:. 1)
                        [ (* Cycle 1: Receive Row 0 (Top). Request Row 2. *)
                          row_top <-- bram_read_data
                        ; bram_a_read_addr <-- row_idx.value +:. 1
                        ; bram_b_read_addr <-- row_idx.value +:. 1
                        ; prime_count <--. 2
                        ]
                        [ if_
                            (prime_count.value ==:. 2)
                            [ (* Cycle 2: Receive Row 1 (Mid). *)
                              row_mid <-- bram_read_data
                            ; prime_count <--. 3
                            ]
                            [ (* Cycle 3: Receive Row 2 (Bot). Ready to scan. *)
                              row_bot <-- bram_read_data
                            ; col_idx <--. 1
                            ; output_row <-- row_mid.value (* Start with original row *)
                            ; prime_count <--. 0
                            ; sm.set_next Scan
                            ]
                        ]
                    ]
                ]
            ] )
        ;
          (* Process one cell per cycle *)
          ( Scan
          , [ (* Compute new cell value: Part 1 keeps original, Part 2 uses survival *)
              let new_cell = mux2 part1_mode.value center survives in
              proc
                [ output_row <-- set_bit output_row.value col_idx.value new_cell
                ; (* Count weak cells *)
                  when_
                    is_weak
                    [ if_
                        part1_mode.value
                        [ part1_acc <-- part1_acc.value +:. 1 ]
                        [ part2_acc <-- part2_acc.value +:. 1
                        ; changes_this_gen <--. 1
                        ]
                    ]
                ; (* Advance to next column or finish row *)
                  if_
                    (col_idx.value +:. 2 <=: grid_width.value) (* col+1 <= width-2 *)
                    [ incr col_idx ]
                    [ sm.set_next Write_row ]
                ]
            ] )
        ;
          (* Write processed row to output BRAM, advance to next row *)
          ( Write_row
          , [ (* Part 2: write to output buffer *)
              when_
                (~:(part1_mode.value))
                [ if_
                    active_buffer.value
                    [ bram_a_write_enable <--. 1
                    ; bram_a_write_addr <-- row_idx.value
                    ; bram_a_write_data <-- output_row.value
                    ]
                    [ bram_b_write_enable <--. 1
                    ; bram_b_write_addr <-- row_idx.value
                    ; bram_b_write_data <-- output_row.value
                    ]
                ]
            ; (* Check if more rows to process *)
              if_
                (row_idx.value +:. 2 <=: grid_height.value) (* row+1 <= height-2 *)
                [ (* Shift rows and load next *)
                  row_top
                  <-- mux2 part1_mode.value row_mid.value output_row.value (* For Part 2, use modified row *)
                ; row_mid <-- row_bot.value
                ; incr row_idx
                ; bram_a_read_addr <-- row_idx.value +:. 2 (* Prefetch next row_bot *)
                ; bram_b_read_addr <-- row_idx.value +:. 2
                ; col_idx <--. 1
                ; prime_count <--. 0
                ; is_shifting <--. 1
                ; sm.set_next Prime
                ]
                [ (* All rows done for this pass *)
                  if_
                    part1_mode.value
                    [ (* Part 1 done, start Part 2 *)
                      part1_mode <--. 0
                    ; row_idx <--. 1
                    ; changes_this_gen <--. 0
                    ; bram_a_read_addr <--. 0
                    ; bram_b_read_addr <--. 0
                    ; prime_count <--. 0
                    ; is_shifting <--. 0
                    ; sm.set_next Prime
                    ]
                    [ (* Part 2: check if stable *)
                      if_
                        changes_this_gen.value
                        [ (* More generations needed - swap buffers *)
                          active_buffer <-- ~:(active_buffer.value)
                        ; row_idx <--. 1
                        ; changes_this_gen <--. 0
                        ; bram_a_read_addr <--. 0
                        ; bram_b_read_addr <--. 0
                        ; prime_count <--. 0
                        ; is_shifting <--. 0
                        ; sm.set_next Prime
                        ]
                        [ (* Stable - done *)
                          sm.set_next Done
                        ]
                    ]
                ]
            ] )
        ;
          (Done, [])
        ]
    ];
  { O.ram_read_addr = uresize load_row_idx.value ~width:addr_bits
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
  Scoped.hierarchical ~scope ~name:"day04_solver" create
;;
