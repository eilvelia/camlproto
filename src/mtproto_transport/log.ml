open! Base

let src = Logs.Src.create "camlproto.transport"

include (val Logs.src_log src : Logs.LOG)
