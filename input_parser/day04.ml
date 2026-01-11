(** Day 04 input parser.

    Parses a grid of '.' and '@' characters, adds a border of '.' around all sides and
    encodes each row as a bit vector ('.' = 0, '@' = 1) Returns (width, height,
    row_strings) where width/height include the border *)

open! Core
open! Util

(** Parse input and add border. Returns (width, height, rows) where each row is a string
    of '0' and '1' characters (for later encoding to bits). Width and height include the
    1-cell border on all sides. *)
let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let lines = List.map lines ~f:String.strip in
  let orig_height = List.length lines in
  let orig_width = if orig_height > 0 then String.length (List.hd_exn lines) else 0 in
  let width = orig_width + 2 in
  let height = orig_height + 2 in
  (* Convert a line to binary string with border *)
  let encode_line line =
    let chars =
      "0" (* left border *)
      ^ String.concat_map line ~f:(fun c ->
        match c with
        | '.' -> "0"
        | '@' -> "1"
        | _ -> failwith (sprintf "Unknown char: %c" c))
      ^ "0" (* right border *)
    in
    chars
  in
  (* Border row is all zeros *)
  let border_row = String.make width '0' in
  let rows = [ border_row ] @ List.map lines ~f:encode_line @ [ border_row ] in
  width, height, rows
;;

(** Parse from raw string. *)
let parse_string ?(verbose = false) raw =
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let lines = List.map lines ~f:String.strip in
  let orig_height = List.length lines in
  let orig_width = if orig_height > 0 then String.length (List.hd_exn lines) else 0 in
  let width = orig_width + 2 in
  let height = orig_height + 2 in
  let encode_line line =
    let chars =
      "0"
      ^ String.concat_map line ~f:(fun c ->
        match c with
        | '.' -> "0"
        | '@' -> "1"
        | _ -> failwith (sprintf "Unknown char: %c" c))
      ^ "0"
    in
    chars
  in
  let border_row = String.make width '0' in
  let rows = [ border_row ] @ List.map lines ~f:encode_line @ [ border_row ] in
  width, height, rows
;;
