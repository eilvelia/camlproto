open! Base

val pp_header: Formatter.t -> Logs.level * Logs.src * string option -> unit
val reporter: ?app:Formatter.t -> ?dst:Formatter.t -> unit -> Logs.reporter
