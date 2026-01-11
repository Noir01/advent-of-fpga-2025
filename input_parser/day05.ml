(** Day 05 input parser.

    Parses input with two sections:
    1. Ranges in format "start-end" (one per line)
    2. Numbers (one per line, after a blank line separator)

    Returns (num_ranges, num_numbers, ranges, numbers) where:
    - ranges is a list of (start, end) int64 tuples
    - numbers is a list of int64 values *)

open! Core
open! Util

let parse_impl raw =
  let lines = String.split_lines raw in
  let lines = List.map lines ~f:String.strip in
  (* Find the empty line that separates ranges from numbers *)
  let rec split_at_empty acc = function
    | [] -> List.rev acc, []
    | "" :: rest -> List.rev acc, rest
    | line :: rest -> split_at_empty (line :: acc) rest
  in
  let range_lines, number_lines = split_at_empty [] lines in
  (* Filter out any remaining empty lines *)
  let range_lines = List.filter range_lines ~f:(fun s -> not (String.is_empty s)) in
  let number_lines = List.filter number_lines ~f:(fun s -> not (String.is_empty s)) in
  (* Parse ranges *)
  let ranges =
    List.map range_lines ~f:(fun line ->
      match String.lsplit2 line ~on:'-' with
      | Some (start_s, end_s) ->
        let start_v = Int64.of_string start_s in
        let end_v = Int64.of_string end_s in
        start_v, end_v
      | None -> failwith (sprintf "Invalid range format: %s" line))
  in
  (* Parse numbers *)
  let numbers = List.map number_lines ~f:Int64.of_string in
  let num_ranges = List.length ranges in
  let num_numbers = List.length numbers in
  num_ranges, num_numbers, ranges, numbers
;;

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  parse_impl raw
;;

let parse_string ?(verbose = false) raw =
  if verbose then print_endline raw;
  parse_impl raw
;;
