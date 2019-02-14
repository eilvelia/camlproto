open! Base

module type DC_LIST = sig
  type dc = string * string
  (** [(addres, port)] *)

  type t = dc list
  val default : t
  val default_dc : dc
  val default_test : t
  val default_test_dc : dc
end

module DcList = struct
  type dc = string * string
  type t = dc list
  let default: t = [
    ("149.154.175.50", "443");
    ("149.154.167.51", "443");
    ("149.154.175.100", "443");
    ("149.154.167.91", "443");
    ("149.154.171.5", "443");
  ]
  let default_dc: dc = ("149.154.167.51", "443")
  let default_test: t = [
    ("149.154.175.10", "443");
    ("149.154.167.40", "443");
    ("149.154.175.117", "443");
  ]
  let default_test_dc: dc = ("149.154.167.40", "443")
end

(* module ProdDcList = struct
  type dc = string * string
  type t = dc list
  let list: t = [
    ("149.154.175.50", "443");
    ("149.154.167.51", "443");
    ("149.154.175.100", "443");
    ("149.154.167.91", "443");
    ("149.154.171.5", "443");
  ]
  let default_dc: dc = ("149.154.167.51", "443")
end

module TestDcList = struct
  type dc = string * string
  type t = dc list
  let list: t = [
    ("149.154.175.10", "443");
    ("149.154.167.40", "443");
    ("149.154.175.117", "443");
  ]
  let default_dc: dc = ("149.154.167.40", "443")
end *)
