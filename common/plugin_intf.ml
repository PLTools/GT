type plugin_args = (Ppxlib.longident * Ppxlib.expression) list

(* type 'a decl_info = string list * string * 'a MidiAst.typ *)

class virtual ['loc, 'typ, 'type_arg, 'ctf, 'cf, 'str, 'sign ] typ_g = object
  method virtual plugin_name : string

  method virtual default_inh : loc:'loc -> Ppxlib.type_declaration -> 'typ

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn  : loc:'loc -> Ppxlib.type_declaration -> 'typ
  method virtual syn_of_param : loc:'loc -> string -> 'typ
  method virtual inh_of_param : Ppxlib.type_declaration -> string -> 'typ

  (* The parameters that the plugin class will have in its definition.
   * Add ['extra] manually if needed *)
  method virtual plugin_class_params: Ppxlib.type_declaration -> 'type_arg list

  (* Arguments of inherit class field that will be generated using the types
   * applied in the RHS of type definition *)
  method virtual prepare_inherit_typ_params_for_alias: loc:'loc ->
    Ppxlib.type_declaration -> Ppxlib.core_type list -> 'typ list

  method virtual extra_class_sig_members: Ppxlib.type_declaration -> 'ctf list
  method virtual extra_class_str_members: Ppxlib.type_declaration -> 'cf  list


  (* These methods will be implemented in plugin.ml *)
  method virtual do_single_sig :
    loc:'loc ->
    is_rec:bool ->
    Ppxlib.type_declaration ->
    'sign list
  method virtual do_single :
    loc:'loc ->
    is_rec:bool ->
    Ppxlib.type_declaration ->
    'str list

  method virtual make_trans_function_name: Ppxlib.type_declaration -> string
  method virtual make_trans_function_typ : loc:'loc -> Ppxlib.type_declaration -> 'typ

  method virtual do_mutals :
    loc:'loc ->
    is_rec:bool ->
    Ppxlib.type_declaration list -> 'str list
end

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers

class virtual g = object
  inherit [loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] typ_g
  (* method virtual plugin_name : string
   *
   * method virtual default_inh : loc:loc -> Ppxlib.type_declaration -> Typ.t
   *
   * (\* synthethized attribute for whole type declaration *\)
   * method virtual default_syn  : loc:loc -> Ppxlib.type_declaration -> Typ.t
   * method virtual syn_of_param : loc:loc -> string -> Typ.t
   * method virtual inh_of_param : Ppxlib.type_declaration -> string -> Typ.t
   *
   * (\* The parameters that the plugin class will have in its definition.
   *  * Add ['extra] manually if needed *\)
   * method virtual plugin_class_params: Ppxlib.type_declaration ->  type_arg list
   *
   * (\* Arguments of inherit class field that will be generated using the types
   *  * applied in the RHS of type definition *\)
   * method virtual prepare_inherit_typ_params_for_alias: loc:loc ->
   *   Ppxlib.type_declaration -> Ppxlib.core_type list -> Typ.t list
   *
   * method virtual extra_class_sig_members: Ppxlib.type_declaration -> Ctf.t list
   * method virtual extra_class_str_members: Ppxlib.type_declaration -> Cf.t list
   *
   *
   * (\* These methods will be implemented in plugin.ml *\)
   * method virtual do_single_sig :
   *   loc:loc ->
   *   is_rec:bool ->
   *   Ppxlib.type_declaration ->
   *   Sig.t list
   * method virtual do_single :
   *   loc:loc ->
   *   is_rec:bool ->
   *   Ppxlib.type_declaration ->
   *   Str.t list
   *
   * method virtual make_trans_function_name: Ppxlib.type_declaration -> string
   * method virtual make_trans_function_typ : loc:loc -> Ppxlib.type_declaration -> Typ.t
   *
   * method virtual do_mutals :
   *   loc:loc ->
   *   is_rec:bool ->
   *   Ppxlib.type_declaration list -> Str.t list *)
end

end


module type PluginRes =
  functor (AstHelpers : GTHELPERS_sig.S) ->
  sig
    open AstHelpers
    val plugin_name : string
    (* class virtual g : object
     *   inherit [AstHelpers.loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] typ_g
     * end *)
      (* object
       *
       *   method virtual default_inh :
       *     loc:AstHelpers.loc -> Ppxlib.type_declaration -> AstHelpers.Typ.t
       *   method virtual default_syn :
       *     loc:AstHelpers.loc -> Ppxlib.type_declaration -> AstHelpers.Typ.t
       *   method virtual do_mutals :
       *     loc:AstHelpers.loc ->
       *     is_rec:bool ->
       *     Ppxlib.type_declaration list -> AstHelpers.Str.t list
       *   method virtual do_single :
       *     loc:AstHelpers.loc ->
       *     is_rec:bool -> Ppxlib.type_declaration -> AstHelpers.Str.t list
       *   method virtual do_single_sig :
       *     loc:AstHelpers.loc ->
       *     is_rec:bool -> Ppxlib.type_declaration -> AstHelpers.Sig.t list
       *   method virtual extra_class_sig_members :
       *     Ppxlib.type_declaration -> AstHelpers.Ctf.t list
       *   method virtual extra_class_str_members :
       *     Ppxlib.type_declaration -> AstHelpers.Cf.t list
       *   method virtual inh_of_param :
       *     Ppxlib.type_declaration -> string -> AstHelpers.Typ.t
       *   method virtual make_trans_function_name :
       *     Ppxlib.type_declaration -> string
       *   method virtual make_trans_function_typ :
       *     loc:AstHelpers.loc -> Ppxlib.type_declaration -> AstHelpers.Typ.t
       *   method virtual plugin_class_params :
       *     Ppxlib.type_declaration -> AstHelpers.type_arg list
       *   method virtual plugin_name : string
       *   method virtual prepare_inherit_typ_params_for_alias :
       *     loc:AstHelpers.loc ->
       *     Ppxlib.type_declaration ->
       *     Ppxlib.core_type list -> AstHelpers.Typ.t list
       *   method virtual syn_of_param :
       *     loc:AstHelpers.loc -> string -> AstHelpers.Typ.t
       * end *)
    val g : plugin_args -> (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) typ_g
  end
