open Ppxlib

type config_plugin = Skip | Use of (longident * expression) list

(* TODO: wtf is the path *)
val str_type_decl :
  loc:Location.t -> path:string ->
  ?use_show:config_plugin ->
  ?use_gmap:config_plugin ->
  ?use_foldl:config_plugin ->
  ?use_show_type:config_plugin ->
  ?use_compare:config_plugin ->
  Asttypes.rec_flag * type_declaration list ->
  structure

(** The same as [str_type_decl] but labels omitted because type_conv doesnt support
   that interface *)
val str_type_decl_implicit :
  loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  structure

(* Declarations in the interface can't get any special arguments for now *)
val sig_type_decl_implicit :
  loc:Location.t -> path:string ->
  Asttypes.rec_flag * type_declaration list ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  signature
