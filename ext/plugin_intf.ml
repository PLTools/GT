(* open Ppxlib *)

type plugin_args = (Ppxlib.longident * Ppxlib.expression) list

type 'a decl_info = string list * string * 'a MidiAst.typ

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers

class virtual ['a] t = object
  method virtual plugin_name : string

  method virtual default_inh : loc:loc -> 'a decl_info -> Typ.t

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn  : loc:loc -> 'a decl_info -> Typ.t
  method virtual syn_of_param : loc:loc -> string -> Typ.t
  method virtual inh_of_param : 'a decl_info -> string -> Typ.t

  (* The parameters that the plugin class will have in its definition.
   * Add ['extra] manually if needed *)
  method virtual plugin_class_params: 'a decl_info -> Typ.t list

  (* Arguments of inherit class field that will be generated using the types
   * applied in the RHS of type definition *)
  method virtual prepare_inherit_typ_params_for_alias: loc:loc ->
    'a decl_info -> Typ.t list -> Typ.t list

  method virtual extra_class_sig_members: 'a decl_info -> Ctf.t list
  method virtual extra_class_str_members: 'a decl_info -> Cf.t list


  (* These methods will be implemented in plugin.ml *)
  method virtual do_single_sig :
    loc:loc ->
    is_rec:bool ->
    'a decl_info ->
    Sig.t list
  method virtual do_single :
    loc:loc ->
    is_rec:bool ->
    'a decl_info ->
    Str.t list

  method virtual make_trans_function_name: 'a decl_info -> string
  method virtual make_trans_function_typ : 'a decl_info -> Typ.t

  method virtual do_mutals :
    loc:loc ->
    is_rec:bool ->
    'a decl_info list -> Str.t list
end

end
