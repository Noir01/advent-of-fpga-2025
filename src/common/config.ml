open! Core

module Config = struct
  (** UART baud rate in bits/second *)
  let baud_rate = 115200

  (** FPGA clock frequency in Hz. *)
  let clock_freq = 100_000_000

  (** Clock cycles per UART bit. *)
  let clocks_per_bit = clock_freq / baud_rate

  (** Maximum input buffer size in bytes. *)
  let max_input_bytes = 16384

  (** Protocol markers *)
  let start_marker = 0x01

  let end_marker = 0xFF
end

(** Per-day configuration. Each day specifies its data layout. *)
module type Day_config = sig
  val data_width : int
  val addr_bits : int
  val bytes_per_word : int
end

(** Day 01: 16-bit words (1 bit direction + 15 bits distance) *)
module Day01_config : Day_config = struct
  let data_width = 16
  let addr_bits = 14
  let bytes_per_word = 2
end

(** Helper to compute address bits from max entries *)
let addr_bits_for_entries n = Int.ceil_log2 (max n 1)
