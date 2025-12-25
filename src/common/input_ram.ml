(** Dual-port block RAM for input data storage.

    One write port (for Loader), one read port (for Solver). *)

open! Core
open! Hardcaml
open! Signal

(** Functor to create RAM with configurable dimensions *)
module Make (Cfg : sig
    val data_width : int
    val addr_bits : int
  end) =
struct
  let data_width = Cfg.data_width
  let addr_bits = Cfg.addr_bits
  let depth = 1 lsl addr_bits

  module I = struct
    type 'a t =
      { clock : 'a
      ; (* Write port *)
        write_enable : 'a
      ; write_addr : 'a [@bits addr_bits]
      ; write_data : 'a [@bits data_width]
      ; (* Read port *)
        read_addr : 'a [@bits addr_bits]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { read_data : 'a [@bits data_width] } [@@deriving hardcaml]
  end

  let create _scope (i : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock:i.clock () in
    (* Dual-port RAM: write port and read port *)
    let read_data =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:depth
        ~write_ports:
          [| { write_clock = i.clock
             ; write_enable = i.write_enable
             ; write_address = i.write_addr
             ; write_data = i.write_data
             }
          |]
        ~read_ports:
          [| { read_clock = i.clock
             ; read_enable = vdd
             ; read_address = i.read_addr
             }
          |]
        ()
    in
    (* Register output for better timing *)
    let read_data = reg spec read_data.(0) in
    { O.read_data }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"ram" create
  ;;
end
