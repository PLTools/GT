open Ppxlib

type plugin_args = (longident * expression) list

class virtual t = object
  method virtual plugin_name : string

  method virtual default_inh : type_declaration -> core_type

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn : type_declaration -> core_type
  method virtual syn_of_param : loc:location -> string  -> core_type
  method virtual inh_of_param : type_declaration -> string -> core_type

  (* The parameters that the plugin class will have in its definition.
   * Add ['extra] manually if needed *)
  method virtual plugin_class_params: type_declaration -> core_type list

  (* Arguments of inherit class field that will be generated using the types
   * applied in the RHS of type definition *)
  method virtual prepare_inherit_typ_params_for_alias: loc:location ->
    type_declaration -> core_type list -> core_type list

  method virtual extra_class_sig_members: type_declaration -> class_type_field list
  method virtual extra_class_str_members: type_declaration -> class_field list


  (* These methods will be implemented in plugin.ml *)
  method virtual do_single_sig :
    loc:location ->
    is_rec:bool ->
    type_declaration ->
    signature_item list
  method virtual do_single :
    loc:location ->
    is_rec:bool ->
    type_declaration ->
    structure_item list

  method virtual make_trans_function_name: type_declaration -> string
  method virtual make_trans_function_typ : type_declaration -> core_type

  method virtual do_mutals :
    loc:location ->
    is_rec:bool ->
    type_declaration list -> structure_item list
end
