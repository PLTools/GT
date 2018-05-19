open Ppxlib

type config_plugin = Skip | Use of (longident * expression) list

module Make : functor (Helpers: GTHELPERS_sig.S) -> sig

open Helpers
(* TODO: wtf is the path *)
(* val str_type_decl :
 *   loc:loc -> path:string ->
 *   Str.t list ->
 *   ?use_show:config_plugin ->
 *   ?use_gmap:config_plugin ->
 *   ?use_foldl:config_plugin ->
 *   ?use_show_type:config_plugin ->
 *   ?use_compare:config_plugin ->
 *   ?use_eq:config_plugin ->
 *   Asttypes.rec_flag * type_declaration list ->
 *   Str.t list *)

val str_type_decl_many_plugins: loc:loc ->
  Str.t list ->
  (Base.string * config_plugin) list ->
  Ppxlib.rec_flag * Ppxlib.type_declaration list ->
  Str.t HelpersBase.List.t

val sig_type_decl_many_plugins: loc:loc ->
  Sig.t list ->
  (Base.string * config_plugin) list ->
  Ppxlib.rec_flag * Ppxlib.type_declaration list ->
  Sig.t HelpersBase.List.t

(* val sig_type_decl :
 *   loc:loc -> path:string ->
 *   Sig.t list ->
 *   ?use_show:config_plugin ->
 *   ?use_gmap:config_plugin ->
 *   ?use_foldl:config_plugin ->
 *   ?use_show_type:config_plugin ->
 *   ?use_compare:config_plugin ->
 *   ?use_eq:config_plugin ->
 *   Asttypes.rec_flag * type_declaration list ->
 *   Sig.t list *)

(* (\** The same as [str_type_decl] but labels omitted because type_conv doesnt support
 *    that interface *\)
 * val str_type_decl_implicit :
 *   loc:loc -> path:string ->
 *   Str.t list ->
 *   config_plugin ->
 *   config_plugin ->
 *   config_plugin ->
 *   config_plugin ->
 *   config_plugin ->
 *   config_plugin ->
 *   Asttypes.rec_flag * type_declaration list ->
 *   Str.t list
 *
 * (\* Declarations in the interface can't get any special arguments for now *\)
 * val sig_type_decl_implicit :
 *   loc:loc -> path:string ->
 *   Sig.t list ->
 *   bool ->
 *   bool ->
 *   bool ->
 *   bool ->
 *   bool ->
 *   bool ->
 *   Asttypes.rec_flag * type_declaration list ->
 *   Sig.t list *)

end

val register_plugin: string -> (module Plugin_intf.PluginRes) -> unit
val get_registered_plugins: unit -> string list

val notify: ('a, unit, string, Base.unit) format4 -> 'a
