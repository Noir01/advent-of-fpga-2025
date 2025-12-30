open! Core
open! Hardcaml
open! Hardcaml_circuits
open! Signal

let data_width = 34 (* each input is < 10^10 *)
let addr_bits = 8
let result_width = 64

module Interfaces = Solver_intf.Make_interfaces (struct
    let data_width = data_width
    let addr_bits = addr_bits
    let result_width = result_width
  end)

module I = Interfaces.I
module O = Interfaces.O

module Divider = Divider.Make (struct
    let width = 64
    let signedness = Signedness.Unsigned
    let architecture = Divider.Architecture.Iterative
  end)

module States = struct
  type t =
    | Idle (** Wait for start signal *)
    | Read_start (** Issue RAM read for start value *)
    | Wait_start (** Wait for RAM, latch start *)
    | Read_end (** Issue RAM read for end value *)
    | Wait_end (** Wait for RAM, latch end *)
    | Compute_len (** Determine digit length of start/end *)
    | Check_len (** Check if current len is valid *)
    | Init_prime (** Initialize inner loop over primes *)
    | Load_f (** Load f value from lookup table *)
    | Compute_bounds (** Compute effectiveStart, effectiveEnd *)
    | Div_a_start (** Start division for ceiling(effectiveStart/f) *)
    | Div_a_wait (** Wait for division result -> a *)
    | Div_b_start (** Start division for floor(effectiveEnd/f) *)
    | Div_b_wait (** Wait for division result -> b *)
    | Compute_terms (** Compute termB and termA, accumulate *)
    | Next_prime (** Move to next prime factor *)
    | Next_len (** Move to next digit length *)
    | Next_pair (** Move to next input pair *)
    | Done (** Computation complete *)
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* POW10 lookup table: 10^i for i = 0..10 *)
let pow10_values =
  [| 1L
   ; 10L
   ; 100L
   ; 1000L
   ; 10000L
   ; 100000L
   ; 1000000L
   ; 10000000L
   ; 100000000L
   ; 1000000000L
   ; 10000000000L
  |]
;;

(* F values indexed by (len-2, variant) where variant: 0=prime0, 1=prime1, 2=exclusion
   For len with single prime factor, only index 0 is valid.
   For len=6 and len=10, all three are valid. *)
let f_values =
  [| (* len=2, prime=2, digits=1 *) [| 11L; 0L; 0L |]
   ; (* len=3, prime=3, digits=1 *) [| 111L; 0L; 0L |]
   ; (* len=4, prime=2, digits=2 *) [| 101L; 0L; 0L |]
   ; (* len=5, prime=5, digits=1 *) [| 11111L; 0L; 0L |]
   ; (* len=6, prime0=2->digits=3, prime1=3->digits=2 *) [| 1001L; 10101L; 111111L |]
   ; (* len=7, prime=7, digits=1 *) [| 1111111L; 0L; 0L |]
   ; (* len=8, prime=2, digits=4 *) [| 10001L; 0L; 0L |]
   ; (* len=9, prime=3, digits=3 *) [| 1001001L; 0L; 0L |]
   ; (* len=10, prime0=2->digits=5, prime1=5->digits=2 *) [| 100001L; 101010101L; 1111111111L |]
  |]
;;

let create scope (i : _ I.t) : _ O.t =
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let pow10 =
    Array.map pow10_values ~f:(fun v ->
      of_int64_trunc ~width:data_width v -- [%string "pow10_%{v#Int64}"])
  in
  (* Build f_value signal lookup table - flatten to 1D array *)
  let f_table =
    Array.concat_mapi f_values ~f:(fun len_idx row ->
      Array.mapi row ~f:(fun var_idx v ->
        of_int64_trunc ~width:data_width v
        -- [%string "f_len%{(len_idx + 2)#Int}_var%{var_idx#Int}"]))
  in
  let%hw_var read_addr = Variable.reg spec ~width:addr_bits in
  let%hw_var range_start = Variable.reg spec ~width:data_width in
  let%hw_var range_end = Variable.reg spec ~width:data_width in
  let%hw_var len_end = Variable.reg spec ~width:4 in
  let%hw_var current_len = Variable.reg spec ~width:4 in
  let%hw_var prime_idx = Variable.reg spec ~width:2 in
  let%hw_var f_value = Variable.reg spec ~width:data_width in
  let%hw_var effective_start = Variable.reg spec ~width:data_width in
  let%hw_var effective_end = Variable.reg spec ~width:data_width in
  let%hw_var a_value = Variable.reg spec ~width:data_width in
  let%hw_var b_value = Variable.reg spec ~width:data_width in
  let%hw_var part1_accum = Variable.reg spec ~width:128 in
  let%hw_var part2_accum = Variable.reg spec ~width:128 in
  (* Divider control *)
  let%hw_var div_start_pulse = Variable.reg spec ~width:1 in
  let%hw_var div_numerator = Variable.reg spec ~width:64 in
  let%hw_var div_denominator = Variable.reg spec ~width:64 in
  (* Compute digit length of a value: returns 1-11 *)
  let digit_length value =
    let open Signal in
    mux2
      (value >=: pow10.(10))
      (of_int_trunc ~width:4 11)
      (mux2
         (value >=: pow10.(9))
         (of_int_trunc ~width:4 10)
         (mux2
            (value >=: pow10.(8))
            (of_int_trunc ~width:4 9)
            (mux2
               (value >=: pow10.(7))
               (of_int_trunc ~width:4 8)
               (mux2
                  (value >=: pow10.(6))
                  (of_int_trunc ~width:4 7)
                  (mux2
                     (value >=: pow10.(5))
                     (of_int_trunc ~width:4 6)
                     (mux2
                        (value >=: pow10.(4))
                        (of_int_trunc ~width:4 5)
                        (mux2
                           (value >=: pow10.(3))
                           (of_int_trunc ~width:4 4)
                           (mux2
                              (value >=: pow10.(2))
                              (of_int_trunc ~width:4 3)
                              (mux2
                                 (value >=: pow10.(1))
                                 (of_int_trunc ~width:4 2)
                                 (of_int_trunc ~width:4 1))))))))))
  in
  (* Lookup f value: index = (current_len - 2) * 3 + prime_idx *)
  let f_table_index =
    let len_minus_2 = uresize (current_len.value -:. 2) ~width:5 in
    let times_3 = len_minus_2 +: sll len_minus_2 ~by:1 in
    times_3 +: uresize prime_idx.value ~width:5
  in
  let f_lookup = mux f_table_index (Array.to_list f_table) in
  let pow10_len = mux current_len.value (Array.to_list pow10) in
  let pow10_len_minus_1 = mux (current_len.value -:. 1) (Array.to_list pow10) in
  (* Check if current length has a second prime factor *)
  let has_second = current_len.value ==:. 6 |: (current_len.value ==:. 10) in
  (* Check if current length is even (for part 1 - only prime 2) *)
  let is_even_len = ~:(lsb current_len.value) in
  let div_inputs : _ Divider.I.t =
    { clock = i.clock
    ; clear = i.clear
    ; numerator = div_numerator.value
    ; denominator = div_denominator.value
    ; start = div_start_pulse.value
    }
  in
  let div_outputs = Divider.hierarchical ~name:"day02_divider" scope div_inputs in
  (* Compute arithmetic series sum combinationally when in Compute_terms *)
  (* term_b = b * (b + 1), term_a = a * (a - 1) *)
  let b_ext = uresize b_value.value ~width:68 in
  let b_plus_1 = b_ext +:. 1 in
  let term_b = b_ext *: b_plus_1 in
  let a_ext = uresize a_value.value ~width:68 in
  let a_minus_1 = a_ext -:. 1 in
  let term_a = a_ext *: a_minus_1 in
  (* diff = term_b - term_a *)
  let diff = term_b -: term_a in
  (* product = f * diff, then divide by 2 *)
  let f_ext = uresize f_value.value ~width:102 in
  let diff_ext = uresize diff ~width:102 in
  let product = f_ext *: diff_ext in
  let series_result = srl product ~by:1 in
  compile
    [ (* Default: clear divider start pulse each cycle *)
      div_start_pulse <--. 0
    ; sm.switch
        [ ( Idle
          , [ read_addr <--. 0
            ; part1_accum <--. 0
            ; part2_accum <--. 0
            ; when_ i.start [ sm.set_next Read_start ]
            ] )
        ; (Read_start, [ sm.set_next Wait_start ])
        ; ( Wait_start
          , [ range_start <-- i.ram_read_data; incr read_addr; sm.set_next Read_end ] )
        ; (Read_end, [ sm.set_next Wait_end ])
        ; ( Wait_end
          , [ range_end <-- i.ram_read_data
            ; incr read_addr
            ; sm.set_next Compute_len
            ] )
        ; ( Compute_len
          , [ len_end <-- digit_length range_end.value
            ; current_len <-- digit_length range_start.value
            ; sm.set_next Check_len
            ] )
        ; ( Check_len
          , [ (* If current_len > len_end, go to next pair *)
              if_
                (current_len.value >: len_end.value)
                [ sm.set_next Next_pair ]
                [ (* If current_len == 1, skip to next length *)
                  if_
                    (current_len.value ==:. 1)
                    [ current_len <-- current_len.value +:. 1; sm.set_next Check_len ]
                    [ sm.set_next Init_prime ]
                ]
            ] )
        ; (Init_prime, [ prime_idx <--. 0; sm.set_next Load_f ])
        ; ( Load_f
          , [ f_value <-- f_lookup
            ; (* Compute effective bounds *)
              (* effective_start = max(range_start, pow10[len-1]) *)
              if_
                (range_start.value >: pow10_len_minus_1)
                [ effective_start <-- range_start.value ]
                [ effective_start <-- pow10_len_minus_1 ]
            ; (* effective_end = min(range_end, pow10[len] - 1) *)
              if_
                (range_end.value <: pow10_len -:. 1)
                [ effective_end <-- range_end.value ]
                [ effective_end <-- pow10_len -:. 1 ]
            ; sm.set_next Compute_bounds
            ] )
        ; ( Compute_bounds
          , [ (* Check if f_value is 0 - skip this prime *)
              if_
                (f_value.value ==:. 0)
                [ sm.set_next Next_prime ]
                [ (* Start division for a = ceil(effective_start / f) *)
                  (* ceil(x/y) = (x + y - 1) / y *)
                  div_numerator
                  <-- uresize
                        (effective_start.value +: f_value.value -:. 1)
                        ~width:64
                ; div_denominator <-- uresize f_value.value ~width:64
                ; div_start_pulse <--. 1
                ; sm.set_next Div_a_start
                ]
            ] )
        ; (Div_a_start, [ sm.set_next Div_a_wait ])
        ; ( Div_a_wait
          , [ when_
                div_outputs.valid
                [ a_value <-- sel_bottom div_outputs.quotient ~width:data_width
                ; (* Start division for b = floor(effective_end / f) *)
                  div_numerator <-- uresize effective_end.value ~width:64
                ; div_denominator <-- uresize f_value.value ~width:64
                ; div_start_pulse <--. 1
                ; sm.set_next Div_b_start
                ]
            ] )
        ; (Div_b_start, [ sm.set_next Div_b_wait ])
        ; ( Div_b_wait
          , [ when_
                div_outputs.valid
                [ b_value <-- sel_bottom div_outputs.quotient ~width:data_width
                ; sm.set_next Compute_terms
                ]
            ] )
        ; ( Compute_terms
          , [ (* Check if b >= a, otherwise no contribution *)
              if_
                (b_value.value <: a_value.value)
                [ sm.set_next Next_prime ]
                [ (* Part 1: only accumulate for prime_idx=0 and even lengths *)
                  when_
                    (prime_idx.value ==:. 0 &: is_even_len)
                    [ part1_accum
                      <-- part1_accum.value +: uresize series_result ~width:128
                    ]
                ; (* Part 2: accumulate with inclusion-exclusion *)
                  (* prime_idx == 2 means exclusion *)
                  if_
                    (prime_idx.value ==:. 2)
                    [ part2_accum
                      <-- part2_accum.value -: uresize series_result ~width:128
                    ]
                    [ part2_accum
                      <-- part2_accum.value +: uresize series_result ~width:128
                    ]
                ; sm.set_next Next_prime
                ]
            ] )
        ; ( Next_prime
          , [ (* Move to next prime or exclusion step *)
              if_
                (prime_idx.value ==:. 0)
                [ (* Check if there's a second prime *)
                  if_
                    has_second
                    [ prime_idx <--. 1; sm.set_next Load_f ]
                    [ sm.set_next Next_len ]
                ]
              @@ elif
                   (prime_idx.value ==:. 1)
                   [ (* After second prime, do exclusion *)
                     prime_idx <--. 2
                   ; sm.set_next Load_f
                   ]
                   [ (* After exclusion, go to next length *)
                     sm.set_next Next_len
                   ]
            ] )
        ; ( Next_len
          , [ incr current_len; sm.set_next Check_len ] )
        ; ( Next_pair
          , [ (* Check if we've processed all input pairs *)
              if_
                (read_addr.value >=: i.input_count)
                [ sm.set_next Done ]
                [ sm.set_next Read_start ]
            ] )
        ; (Done, [])
        ]
    ];
  { O.ram_read_addr = read_addr.value
  ; part1 = uresize ~width:result_width part1_accum.value
  ; part2 = uresize ~width:result_width part2_accum.value
  ; done_ = sm.is Done
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day02_solver" create
;;
