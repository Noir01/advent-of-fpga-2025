(** Common utilities for input parsing.

    Provides UART symbol types and helper functions for converting parsed data into byte
    streams for FPGA simulation. *)

open! Core

module Uart_symbol = struct
  type t =
    | Byte of char
    | Rts of bool
  [@@deriving sexp]
end

module type Input_parser = sig
  val parse : ?verbose:bool -> string -> Uart_symbol.t list
end

(** Get input file contents from the inputs directory *)
let get_input_file filename =
  let path = "inputs/" ^ filename in
  In_channel.read_all path
;;

(** Extract all unsigned integers from a string *)
let all_ints_unsigned s =
  let re = Re.Perl.compile_pat "\\d+" in
  Re.matches re s |> List.map ~f:Int.of_string
;;

(** Extract all signed integers from a string *)
let all_ints_signed s =
  let re = Re.Perl.compile_pat "-?\\d+" in
  Re.matches re s |> List.map ~f:Int.of_string
;;

(** Convert an integer to n bytes in little-endian order *)
let int_to_bytes_le ~n x =
  List.init n ~f:(fun i -> Char.of_int_exn ((x lsr (i * 8)) land 0xFF))
;;

(** Convert an integer to n UART byte symbols in little-endian order *)
let int_to_uart_bytes_le ~n x =
  int_to_bytes_le ~n x |> List.map ~f:(fun x -> Uart_symbol.Byte x)
;;

let%expect_test "test int_to_bytes_le" =
  let bytes = int_to_bytes_le ~n:4 0x12345678 in
  print_s [%message "" ~_:(bytes |> List.map ~f:Char.to_int : int list)];
  [%expect {| (120 86 52 18) |}]
;;
