(** Day 01 input parser.

    Outputs 16-bit words where:
    - Bit 15: direction (0=R/right, 1=L/left)
    - Bits 14-0: distance *)

open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let values =
    List.map lines ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line
      then 0
      else (
        let dir = String.get line 0 in
        let dist = Int.of_string (String.sub line ~pos:1 ~len:(String.length line - 1)) in
        let dir_bit = if Char.equal dir 'L' then 0x8000 else 0 in
        dir_bit lor dist))
  in
  (* Convert each value to 2 bytes (16-bit values) in little-endian order *)
  List.concat_map values ~f:(int_to_uart_bytes_le ~n:2) @ [ Uart_symbol.Rts true ]
;;
