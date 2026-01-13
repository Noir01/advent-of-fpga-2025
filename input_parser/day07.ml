(** Day 07 input parser.

    Parses a grid where each cell can be '.', '^', 'S', or '|'. Encodes each cell as a
    2-bit value:
    - 0: '.' (empty)
    - 1: '^' (caret)
    - 2: 'S' (start)
    - 3: '|' (pipe)

    Returns (width, height, rows) where each row is a list of encoded 2-bit values. *)

open! Core
open! Util

(** Encode a single character to its 2-bit value *)
let encode_char c =
  match c with
  | '.' -> 0
  | '^' -> 1
  | 'S' -> 2
  | '|' -> 3
  | _ -> failwith (sprintf "Unknown char: %c" c)
;;

(** Parse input file. Returns (width, height, rows) where each row is a list of encoded
    2-bit values (as ints). *)
let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let height = List.length lines in
  let width = if height > 0 then String.length (List.hd_exn lines) else 0 in
  let rows =
    List.map lines ~f:(fun line -> String.to_list line |> List.map ~f:encode_char)
  in
  width, height, rows
;;

(** Parse from raw string. Returns (width, height, rows) where each row is a list of
    encoded 2-bit values (as ints). *)
let parse_string ?(verbose = false) raw =
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let height = List.length lines in
  let width =
    if List.is_empty lines
    then 0
    else List.map lines ~f:String.length |> List.fold ~init:0 ~f:max
  in
  let rows =
    List.map lines ~f:(fun line ->
      let padding = List.init (width - String.length line) ~f:(fun _ -> 0) in
      let encoded = String.to_list line |> List.map ~f:encode_char in
      encoded @ padding)
  in
  width, height, rows
;;
