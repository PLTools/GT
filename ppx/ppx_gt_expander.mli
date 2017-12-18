open Ppx_core

val str_type_decl : loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  bool -> bool ->
  structure
