(** Day 02 input parser.

    Outputs 34-bit integers as a flat list *)

open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines =
    String.split_lines raw |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  List.concat_map lines ~f:(fun line ->
    let line = String.strip line in
    let pairs = String.split ~on:',' line in
    List.concat_map pairs ~f:(fun pair ->
      let pair = String.strip pair in
      if String.is_empty pair
      then []
      else (
        match String.lsplit2 ~on:'-' pair with
        | Some (a, b) ->
          [ Int.of_string (String.strip a); Int.of_string (String.strip b) ]
        | None -> failwith ("Invalid pair format: " ^ pair))))
;;
