open! Core
open! Hardcaml
open! Hardcaml_circuits
open! Signal

let data_width = 34 (* each input is < 10^10 *)
let addr_bits = 8
let result_width = 64
let num_units = 4 (* TODO: don't hardcode to 4 *)

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
    | Load_batch (** Read 8 words into 4 (start, end) register pairs *)
    | Wait_load (** Wait for RAM, latch end *)
    | Par_compute_len (** Parallel: All 4 units compute digit lengths *)
    | Par_check_len (** Parallel: Check if current_len > len_end or == 1 *)
    | Par_init_prime (** Parallel: Initialize prime_idx = 0 *)
    | Par_load_f (** Parallel: Load f values, compute effective bounds *)
    | Par_compute_bounds (** Parallel: Check f != 0, prepare division *)
    | Par_div_a_start (** Parallel: Start division for a *)
    | Par_div_a_wait (** Parallel: Wait for all 4 dividers *)
    | Par_div_b_start (** Parallel: Start division for b *)
    | Par_div_b_wait (** Parallel: Wait for all 4 dividers *)
    | Par_compute_terms (** Parallel: Compute series, update local accumulators *)
    | Par_next_prime (** Parallel: Advance prime_idx or increment current_len *)
    | Next_batch (** Move to next batch of 4, or finish *)
    | Reduce (** Sum partial accumulators from all 4 units *)
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
   For len with single prime factor, only index 0 is valid
   For len=6 and len=10, all three are valid *)
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
  (* Shared registers *)
  let%hw_var read_addr = Variable.reg spec ~width:addr_bits in
  let%hw_var load_word_idx = Variable.reg spec ~width:4 in
  let%hw_var part1_accum = Variable.reg spec ~width:128 in
  let%hw_var part2_accum = Variable.reg spec ~width:128 in
  (* Per-unit registers - 4 copies each *)
  let range_start =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let range_end =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let len_end =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:4)
  in
  let current_len =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:4)
  in
  let prime_idx =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:2)
  in
  let f_value =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let effective_start =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let effective_end =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let a_value =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let b_value =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:data_width)
  in
  let unit_done =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:1)
  in
  let part1_local =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:128)
  in
  let part2_local =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:128)
  in
  (* Divider control - 4 copies *)
  let div_start_pulse =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:1)
  in
  let div_numerator =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:64)
  in
  let div_denominator =
    Array.init num_units ~f:(fun _ -> Variable.reg spec ~width:64)
  in
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
  let f_table_index =
    Array.map current_len ~f:(fun cl ->
      Array.map prime_idx ~f:(fun pi ->
        let len_minus_2 = uresize (cl.value -:. 2) ~width:5 in
        let times_3 = len_minus_2 +: sll len_minus_2 ~by:1 in
        times_3 +: uresize pi.value ~width:5))
  in
  let f_lookup =
    Array.mapi f_table_index ~f:(fun idx _ ->
      mux f_table_index.(idx).(idx) (Array.to_list f_table))
  in
  let pow10_len =
    Array.map current_len ~f:(fun cl -> mux cl.value (Array.to_list pow10))
  in
  let pow10_len_minus_1 =
    Array.map current_len ~f:(fun cl -> mux (cl.value -:. 1) (Array.to_list pow10))
  in
  let has_second =
    Array.map current_len ~f:(fun cl -> cl.value ==:. 6 |: (cl.value ==:. 10))
  in
  let is_even_len = Array.map current_len ~f:(fun cl -> ~:(lsb cl.value)) in
  (* Instantiate 4 dividers *)
  let div_outputs =
    Array.mapi div_start_pulse ~f:(fun idx pulse ->
      let div_inputs : _ Divider.I.t =
        { clock = i.clock
        ; clear = i.clear
        ; numerator = div_numerator.(idx).value
        ; denominator = div_denominator.(idx).value
        ; start = pulse.value
        }
      in
      Divider.hierarchical ~name:[%string "day02_divider_%{idx#Int}"] scope div_inputs)
  in
  (* Per-unit series result computation *)
  let series_result =
    Array.mapi a_value ~f:(fun idx a ->
      let b = b_value.(idx) in
      let f = f_value.(idx) in
      let b_ext = uresize b.value ~width:68 in
      let b_plus_1 = b_ext +:. 1 in
      let term_b = b_ext *: b_plus_1 in
      let a_ext = uresize a.value ~width:68 in
      let a_minus_1 = a_ext -:. 1 in
      let term_a = a_ext *: a_minus_1 in
      let diff = term_b -: term_a in
      let f_ext = uresize f.value ~width:102 in
      let diff_ext = uresize diff ~width:102 in
      let product = f_ext *: diff_ext in
      srl product ~by:1)
  in
  let all_dividers_done =
    Array.foldi div_outputs ~init:vdd ~f:(fun idx acc div ->
      acc &: (unit_done.(idx).value |: div.valid))
  in
  let all_units_done =
    Array.fold unit_done ~init:vdd ~f:(fun acc ud -> acc &: ud.value)
  in
  (* Calculate how many words to load this batch *)
  let words_remaining = i.input_count -: uresize read_addr.value ~width:addr_bits in
  let words_this_batch =
    mux2 (words_remaining >=:. 8) (of_int_trunc ~width:4 8) (uresize words_remaining ~width:4)
  in
  compile
    [ (* Clear all divider start pulses each cycle *)
      proc
        (Array.to_list (Array.map div_start_pulse ~f:(fun pulse -> pulse <--. 0)))
    ; sm.switch
        [ ( Idle
          , [ read_addr <--. 0
            ; part1_accum <--. 0
            ; part2_accum <--. 0
            ; proc
                (Array.to_list
                   (Array.map part1_local ~f:(fun p -> p <--. 0))
                 @ Array.to_list (Array.map part2_local ~f:(fun p -> p <--. 0)))
            ; when_ i.start [ sm.set_next Load_batch; load_word_idx <--. 0 ]
            ] )
        ; ( Load_batch
          , (let prev_idx = load_word_idx.value -:. 1 in
             let prev_unit = srl prev_idx ~by:1 in
             let prev_is_start = ~:(lsb prev_idx) in
             [ (* Check if we have any words to load *)
               if_
                 (load_word_idx.value >=: words_this_batch)
                 [ (* All words loaded, wait for last one and initialize units *)
                   sm.set_next Wait_load
                 ]
                 [ (* Issue read for current word, address is read_addr + load_word_idx *)
                   sm.set_next Load_batch
                 ; incr load_word_idx
                 ; (* Latch the previous word (1 cycle latency) *)
                   when_
                     (load_word_idx.value >:. 0)
                     (Array.to_list
                        (Array.mapi range_start ~f:(fun idx rs ->
                           when_
                             (uresize prev_unit ~width:2 ==:. idx &: prev_is_start)
                             [ rs <-- i.ram_read_data ]))
                     @ Array.to_list
                         (Array.mapi range_end ~f:(fun idx re ->
                            when_
                              (uresize prev_unit ~width:2 ==:. idx &: ~:prev_is_start)
                              [ re <-- i.ram_read_data ])))
                 ]
             ]) )
        ; ( Wait_load
          , (let final_idx = load_word_idx.value -:. 1 in
             let final_unit = srl final_idx ~by:1 in
             let final_is_start = ~:(lsb final_idx) in
             Array.to_list
               (Array.mapi range_start ~f:(fun idx rs ->
                  when_
                    (uresize final_unit ~width:2 ==:. idx &: final_is_start)
                    [ rs <-- i.ram_read_data ]))
             @ Array.to_list
                 (Array.mapi range_end ~f:(fun idx re ->
                    when_
                      (uresize final_unit ~width:2 ==:. idx &: ~:final_is_start)
                      [ re <-- i.ram_read_data ]))
             @ Array.to_list
                 (Array.mapi unit_done ~f:(fun idx ud ->
                    let words_needed = of_int_trunc ~width:4 ((idx + 1) * 2) in
                    ud <-- mux2 (words_this_batch >=: words_needed) gnd vdd))
             @ [ read_addr
                 <-- uresize
                       (uresize read_addr.value ~width:12
                       +: uresize words_this_batch ~width:12)
                       ~width:addr_bits
               ; sm.set_next Par_compute_len
               ]) )
        ; ( Par_compute_len
          , [ proc
                (Array.to_list
                   (Array.mapi len_end ~f:(fun idx le ->
                      when_
                        ~:(unit_done.(idx).value)
                        [ le <-- digit_length range_end.(idx).value
                        ; current_len.(idx) <-- digit_length range_start.(idx).value
                        ])))
            ; sm.set_next Par_check_len
            ] )
        ; ( Par_check_len
          , [ proc
                (Array.to_list
                   (Array.mapi unit_done ~f:(fun idx ud ->
                      when_
                        ~:(ud.value)
                        [ (* If current_len > len_end, mark unit as done *)
                          if_
                            (current_len.(idx).value >: len_end.(idx).value)
                            [ ud <--. 1 ]
                            [ (* If current_len == 1, skip to next length *)
                              when_
                                (current_len.(idx).value ==:. 1)
                                [ current_len.(idx)
                                  <-- current_len.(idx).value +:. 1
                                ]
                            ]
                        ])))
            ; (* Check if all units are done or need to continue *)
              if_
                all_units_done
                [ sm.set_next Next_batch ]
                [ (* Check if any unit still has len==1 to skip *)
                  let any_at_len_1 =
                    Array.foldi current_len ~init:gnd ~f:(fun idx acc cl ->
                      acc
                      |: (~:(unit_done.(idx).value)
                         &: (cl.value ==:. 1)
                         &: (cl.value <=: len_end.(idx).value)))
                  in
                  (* Check if any unit needs to continue with primes (pi > 0) *)
                  let any_continuing_prime =
                    Array.foldi prime_idx ~init:gnd ~f:(fun idx acc pi ->
                      acc |: (~:(unit_done.(idx).value) &: (pi.value >:. 0)))
                  in
                  if_
                    any_at_len_1
                    [ sm.set_next Par_check_len ]
                    [ if_
                        any_continuing_prime
                        [ sm.set_next Par_load_f ]
                        [ sm.set_next Par_init_prime ]
                    ]
                ]
            ] )
        ; ( Par_init_prime
          , [ proc
                (Array.to_list
                   (Array.mapi prime_idx ~f:(fun idx pi ->
                      when_ ~:(unit_done.(idx).value) [ pi <--. 0 ])))
            ; sm.set_next Par_load_f
            ] )
        ; ( Par_load_f
          , [ proc
                (Array.to_list
                   (Array.mapi f_value ~f:(fun idx fv ->
                      when_
                        ~:(unit_done.(idx).value)
                        [ fv <-- f_lookup.(idx)
                        ; (* effective_start = max(range_start, pow10[len-1]) *)
                          if_
                            (range_start.(idx).value >: pow10_len_minus_1.(idx))
                            [ effective_start.(idx) <-- range_start.(idx).value ]
                            [ effective_start.(idx) <-- pow10_len_minus_1.(idx) ]
                        ; (* effective_end = min(range_end, pow10[len] - 1) *)
                          if_
                            (range_end.(idx).value <: pow10_len.(idx) -:. 1)
                            [ effective_end.(idx) <-- range_end.(idx).value ]
                            [ effective_end.(idx) <-- pow10_len.(idx) -:. 1 ]
                        ])))
            ; sm.set_next Par_compute_bounds
            ] )
        ; ( Par_compute_bounds
          , [ proc
                (Array.to_list
                   (Array.mapi f_value ~f:(fun idx fv ->
                      when_
                        ~:(unit_done.(idx).value)
                        [ (* If f_value is 0, we can skip in div wait by checking *)
                          (* Start division for a = ceil(effective_start / f) *)
                          if_
                            (fv.value ==:. 0)
                            [ (* Skip division, set a > b to skip accumulation *)
                              a_value.(idx) <--. 1
                            ; b_value.(idx) <--. 0
                            ]
                            [ div_numerator.(idx)
                              <-- uresize
                                    (effective_start.(idx).value
                                    +: fv.value
                                    -:. 1)
                                    ~width:64
                            ; div_denominator.(idx) <-- uresize fv.value ~width:64
                            ; div_start_pulse.(idx) <--. 1
                            ]
                        ])))
            ; sm.set_next Par_div_a_start
            ] )
        ; (Par_div_a_start, [ sm.set_next Par_div_a_wait ])
        ; ( Par_div_a_wait
          , [ when_
                all_dividers_done
                [ proc
                    (Array.to_list
                       (Array.mapi a_value ~f:(fun idx av ->
                          when_
                            (~:(unit_done.(idx).value) &: (f_value.(idx).value <>:. 0))
                            [ av
                              <-- sel_bottom
                                    div_outputs.(idx).quotient
                                    ~width:data_width
                            ; (* Start division for b *)
                              div_numerator.(idx)
                              <-- uresize effective_end.(idx).value ~width:64
                            ; div_denominator.(idx)
                              <-- uresize f_value.(idx).value ~width:64
                            ; div_start_pulse.(idx) <--. 1
                            ])))
                ; sm.set_next Par_div_b_start
                ]
            ] )
        ; (Par_div_b_start, [ sm.set_next Par_div_b_wait ])
        ; ( Par_div_b_wait
          , [ when_
                all_dividers_done
                [ proc
                    (Array.to_list
                       (Array.mapi b_value ~f:(fun idx bv ->
                          when_
                            (~:(unit_done.(idx).value) &: (f_value.(idx).value <>:. 0))
                            [ bv
                              <-- sel_bottom
                                    div_outputs.(idx).quotient
                                    ~width:data_width
                            ])))
                ; sm.set_next Par_compute_terms
                ]
            ] )
        ; ( Par_compute_terms
          , [ proc
                (Array.to_list
                   (Array.mapi part1_local ~f:(fun idx p1 ->
                      when_
                        (~:(unit_done.(idx).value)
                        &: (b_value.(idx).value >=: a_value.(idx).value))
                        [ (* Part 1: only accumulate for prime_idx=0 and even lengths *)
                          when_
                            (prime_idx.(idx).value ==:. 0 &: is_even_len.(idx))
                            [ p1
                              <-- p1.value +: uresize series_result.(idx) ~width:128
                            ]
                        ])))
            ; proc
                (Array.to_list
                   (Array.mapi part2_local ~f:(fun idx p2 ->
                      when_
                        (~:(unit_done.(idx).value)
                        &: (b_value.(idx).value >=: a_value.(idx).value))
                        [ (* Part 2: inclusion-exclusion *)
                          if_
                            (prime_idx.(idx).value ==:. 2)
                            [ p2
                              <-- p2.value -: uresize series_result.(idx) ~width:128
                            ]
                            [ p2
                              <-- p2.value +: uresize series_result.(idx) ~width:128
                            ]
                        ])))
            ; sm.set_next Par_next_prime
            ] )
        ; ( Par_next_prime
          , [ proc
                (Array.to_list
                   (Array.mapi prime_idx ~f:(fun idx pi ->
                      when_
                        ~:(unit_done.(idx).value)
                        [ if_
                            (pi.value ==:. 0)
                            [ if_
                                has_second.(idx)
                                [ pi <--. 1 ]
                                [ (* No second prime, go to next length *)
                                  current_len.(idx)
                                  <-- current_len.(idx).value +:. 1
                                ; pi <--. 0
                                ]
                            ]
                          @@ elif
                               (pi.value ==:. 1)
                               [ pi <--. 2 ]
                               [ (* After exclusion (pi=2), go to next length *)
                                 current_len.(idx)
                                 <-- current_len.(idx).value +:. 1
                               ; pi <--. 0
                               ]
                        ])))
            ; (* Always go to Par_check_len to handle done checks and routing *)
              sm.set_next Par_check_len
            ] )
        ; ( Next_batch
          , [ (* Check if we've processed all input *)
              if_
                (uresize read_addr.value ~width:addr_bits >=: i.input_count)
                [ sm.set_next Reduce ]
                [ (* Reset unit_done flags and start loading next batch *)
                  proc
                    (Array.to_list (Array.map unit_done ~f:(fun ud -> ud <--. 0)))
                ; load_word_idx <--. 0
                ; sm.set_next Load_batch
                ]
            ] )
        ; ( Reduce
          , [ (* Sum all partial accumulators *)
              part1_accum
              <-- Array.fold part1_local ~init:(zero 128) ~f:(fun acc p ->
                    acc +: p.value)
            ; part2_accum
              <-- Array.fold part2_local ~init:(zero 128) ~f:(fun acc p ->
                    acc +: p.value)
            ; sm.set_next Done
            ] )
        ; (Done, [])
        ]
    ];
  (* When latching, we want data for load_word_idx - 1 so that address
     When load_word_idx = 0 (first iteration), so 0 *)
  let effective_load_idx =
    mux2
      (load_word_idx.value >:. 0)
      (load_word_idx.value -:. 1)
      load_word_idx.value
  in
  { O.ram_read_addr =
      uresize read_addr.value ~width:addr_bits
      +: uresize effective_load_idx ~width:addr_bits
  ; part1 = uresize ~width:result_width part1_accum.value
  ; part2 = uresize ~width:result_width part2_accum.value
  ; done_ = sm.is Done
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day02_solver" create
;;
