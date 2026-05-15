(** Translate a bound statement into a physical plan. *)

val plan : Sema.bound_stmt -> Plan.op
