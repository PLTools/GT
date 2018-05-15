open Ppxlib

type config_plugin = Skip | Use of (longident * expression) list

module Make : functor (X: GTHELPERS_sig.S) -> sig

(* TODO: wtf is the path *)
val str_type_decl :
  loc:X.loc -> path:string ->
  X.Str.t list ->
  ?use_show:config_plugin ->
  ?use_gmap:config_plugin ->
  ?use_foldl:config_plugin ->
  ?use_show_type:config_plugin ->
  ?use_compare:config_plugin ->
  ?use_eq:config_plugin ->
  Asttypes.rec_flag * type_declaration list ->
  X.Str.t list

val sig_type_decl :
  loc:X.loc -> path:string ->
  X.Sig.t list ->
  ?use_show:config_plugin ->
  ?use_gmap:config_plugin ->
  ?use_foldl:config_plugin ->
  ?use_show_type:config_plugin ->
  ?use_compare:config_plugin ->
  ?use_eq:config_plugin ->
  Asttypes.rec_flag * type_declaration list ->
  X.Sig.t list

(** The same as [str_type_decl] but labels omitted because type_conv doesnt support
   that interface *)
val str_type_decl_implicit :
  loc:X.loc -> path:string ->
  X.Str.t list ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  config_plugin ->
  Asttypes.rec_flag * type_declaration list ->
  X.Str.t list

(* Declarations in the interface can't get any special arguments for now *)
val sig_type_decl_implicit :
  loc:X.loc -> path:string ->
  X.Sig.t list ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  Asttypes.rec_flag * type_declaration list ->
  X.Sig.t list

end

val register_plugin: string -> (module Plugin_intf.PluginRes) -> unit
