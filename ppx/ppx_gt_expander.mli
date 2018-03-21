open Ppx_core

(* TODO: wtf is the path *)
val str_type_decl :
  loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  ?use_show:bool -> ?use_gmap:bool -> ?use_foldl:bool ->
  ?for1arg:(expression option) ->
  structure

(** The same as [str_type_decl] but labels omitted because type_conv doesnt support
   that interface *)
val str_type_decl_implicit :
  loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  bool -> bool -> bool -> expression option ->
  structure

val sig_type_decl_implicit :
  loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  bool -> bool -> bool -> (* expression option -> *)
  signature
