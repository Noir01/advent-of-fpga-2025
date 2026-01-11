open! Core
open! Hardcaml
open! Signal

module type S = sig
  val data_width : int
  val addr_bits : int
  val result_width : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a (** Begin computation *)
      ; input_count : 'a (** Number of input words in RAM *)
      ; ram_read_data : 'a (** Data from RAM read port *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { ram_read_addr : 'a (** Address for RAM read port *)
      ; part1 : 'a
      ; part2 : 'a
      ; done_ : 'a (** High when computation complete *)
      ; debug_state : 'a
      ; debug_insert_pos : 'a
      ; debug_shift_idx : 'a
      ; debug_bram_read : 'a
      ; debug_cmp_val : 'a
      ; debug_write_addr : 'a
      ; debug_write_data : 'a
      ; debug_read_addr : 'a
      }
    [@@deriving hardcaml]
  end

  (** Create the solver circuit *)
  val create : Scope.t -> Signal.t I.t -> Signal.t O.t

  (** Hierarchical wrapper for proper module naming *)
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Make_interfaces (Cfg : sig
    val data_width : int
    val addr_bits : int
    val result_width : int
  end) =
struct
  let data_width = Cfg.data_width
  let addr_bits = Cfg.addr_bits
  let result_width = Cfg.result_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; input_count : 'a [@bits addr_bits]
      ; ram_read_data : 'a [@bits data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ram_read_addr : 'a [@bits addr_bits]
      ; part1 : 'a [@bits result_width]
      ; part2 : 'a [@bits result_width]
      ; done_ : 'a
      ; debug_state : 'a [@bits 6]
      ; debug_insert_pos : 'a [@bits 16]
      ; debug_shift_idx : 'a [@bits 16]
      ; debug_bram_read : 'a [@bits data_width]
      ; debug_cmp_val : 'a [@bits data_width]
      ; debug_write_addr : 'a [@bits addr_bits]
      ; debug_write_data : 'a [@bits data_width]
      ; debug_read_addr : 'a [@bits addr_bits]
      }
    [@@deriving hardcaml]
  end
end
