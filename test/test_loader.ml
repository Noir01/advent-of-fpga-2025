(** Tests for the Loader module. *)

open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Loader = Advent_of_fpga_2025.Loader.Make (struct
    let data_width = 16
    let addr_bits = 10
    let bytes_per_word = 2
  end)

module Harness = Cyclesim_harness.Make (Loader.I) (Loader.O)

let ( <--. ) = Bits.( <--. )

(** Send a byte to the loader *)
let send_byte (sim : Harness.Sim.t) byte =
  let i = Cyclesim.inputs sim in
  i.uart_byte <--. byte;
  i.uart_valid := Bits.vdd;
  Cyclesim.cycle sim;
  i.uart_valid := Bits.gnd;
  Cyclesim.cycle sim
;;

(** Testbench for loading 3 words *)
let load_3_words_testbench (sim : Harness.Sim.t) =
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  (* Reset *)
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  (* Start the loader *)
  i.start := Bits.vdd;
  Cyclesim.cycle sim;
  i.start := Bits.gnd;
  Cyclesim.cycle sim;
  (* Send start marker *)
  send_byte sim 0x01;
  (* Send word count: 3 words (little-endian) *)
  send_byte sim 0x03;
  (* Low byte *)
  send_byte sim 0x00;
  (* High byte *)
  (* Send 3 words as bytes (each word is 2 bytes, little-endian) *)
  (* Word 1: 0x0102 -> bytes 0x02, 0x01 *)
  send_byte sim 0x02;
  send_byte sim 0x01;
  (* Word 2: 0x0304 -> bytes 0x04, 0x03 *)
  send_byte sim 0x04;
  send_byte sim 0x03;
  (* Word 3: 0x0506 -> bytes 0x06, 0x05 *)
  send_byte sim 0x06;
  send_byte sim 0x05;
  (* Send end marker *)
  send_byte sim 0xFF;
  (* Wait for done *)
  for _ = 1 to 5 do
    Cyclesim.cycle sim
  done;
  let ready = Bits.to_bool !(o.ready) in
  let word_count = Bits.to_int_trunc !(o.word_count) in
  print_s [%message "Result" (ready : bool) (word_count : int)]
;;

(** Testbench for empty message *)
let empty_message_testbench (sim : Harness.Sim.t) =
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  (* Reset *)
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  (* Start the loader *)
  i.start := Bits.vdd;
  Cyclesim.cycle sim;
  i.start := Bits.gnd;
  Cyclesim.cycle sim;
  (* Send start marker *)
  send_byte sim 0x01;
  (* Send word count: 0 words *)
  send_byte sim 0x00;
  send_byte sim 0x00;
  (* Send end marker immediately *)
  send_byte sim 0xFF;
  (* Wait for done *)
  for _ = 1 to 5 do
    Cyclesim.cycle sim
  done;
  let ready = Bits.to_bool !(o.ready) in
  let word_count = Bits.to_int_trunc !(o.word_count) in
  print_s [%message "Result" (ready : bool) (word_count : int)]
;;

let%expect_test "loader receives bytes and assembles 3 words" =
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Loader.hierarchical
    load_3_words_testbench;
  [%expect {| (Result (ready true) (word_count 3)) |}]
;;

let%expect_test "loader handles empty message" =
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Loader.hierarchical
    empty_message_testbench;
  [%expect {| (Result (ready true) (word_count 0)) |}]
;;
