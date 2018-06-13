open Ppxlib

type config_plugin = Skip | Use of (longident * expression) list

module Make : functor (Helpers: GTHELPERS_sig.S) -> sig

open Helpers

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


end

val register_plugin: string -> (module Plugin_intf.PluginRes) -> unit
val get_registered_plugins: unit -> string list

