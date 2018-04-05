open Ppxlib

class virtual t = object
  method virtual plugin_name : string

  method virtual default_inh : core_type

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn : type_declaration -> core_type
  method virtual syn_of_param : loc:location -> string  -> core_type

  (* The parameters that the plugin class will have in its definition.
   * Add ['extra] manually if needed *)
  method virtual plugin_class_params: type_declaration -> core_type list

  (* Arguments of inherit class field that will be generated using the types
   * applied in the RHS of type definition *)
  method virtual prepare_inherit_args_for_alias: loc:location -> type_declaration ->
    core_type list -> core_type list

  method virtual extra_class_sig_members: type_declaration -> class_type_field list
  method virtual extra_class_str_members: type_declaration -> class_field list
end
