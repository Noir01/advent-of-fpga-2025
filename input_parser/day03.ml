(** Day 03 input parser.

    Outputs raw digit strings, one per line. Each string contains digits 1-9.
    Encoding to Bits happens in the runner/test since input_parser doesn't
    depend on Hardcaml. *)

open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines = String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  List.map lines ~f:String.strip
;;
