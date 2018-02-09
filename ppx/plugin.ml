open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

class virtual generator = object(self)
  method virtual plugin_name : string

  method virtual default_inh : core_type
  method virtual default_syn : core_type



  method virtual make_trans_functions: loc:location ->
    is_rec:bool -> string list -> type_declaration list -> structure_item
  method virtual make_class : loc:location ->
    type_declaration -> is_rec:bool -> string list -> structure_item

  method virtual plugin_class_params: type_declaration -> core_type list

  method make_shortend_class ~loc ~(is_rec: bool) mutal_names tdecls =
    List.map tdecls ~f:(fun tdecl ->
        let mutal_names = List.filter mutal_names ~f:(String.(<>) tdecl.ptype_name.txt) in
        let class_name = sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt in
        let stub_name = class_name ^ "_stub" in
        (* maybe it should be called proto *)
        let mut_funcs = List.map ~f:(sprintf "%s_%s" self#plugin_name) mutal_names in
        let real_args = "fself" :: (List.map ~f:((^)"f") @@ make_new_names (List.length tdecl.ptype_params)) in
        let new_params = self#plugin_class_params tdecl in
        Str.single_class ~loc ~name:class_name
          ~wrap:(Cl.fun_list @@ List.map ~f:(Pat.sprintf ~loc "%s") @@ real_args)
          ~params:(invariantize new_params)
          [ Cf.inherit_ ~loc @@ Cl.apply
              (Cl.constr ~loc (Located.lident ~loc stub_name) new_params)
              (List.map ~f:(fun s -> Nolabel, Exp.sprintf ~loc "%s" s) @@ (mut_funcs@real_args))
          ]
      )

  method do_single ~loc ~is_rec tdecl =
    [ self#make_class ~loc ~is_rec tdecl []
    ; self#make_trans_functions ~loc ~is_rec [] [tdecl]
    ]

  method do_mutals ~(loc: Location.t) ~(is_rec: bool) tdecls : structure_item list =
    (* for mutal recursion we need to generate two classes and one function *)
    let mut_names = List.map tdecls ~f:(fun td -> td.ptype_name.txt) in
    List.map tdecls ~f:(fun tdecl ->
        self#make_class ~loc ~is_rec:true tdecl @@
        List.filter mut_names ~f:(String.(<>) tdecl.ptype_name.txt)
      ) @
    (self#make_trans_functions ~loc ~is_rec:true mut_names tdecls) ::
    (self#make_shortend_class  ~loc ~is_rec:true mut_names tdecls)

end

let self_arg_name = "_fself"
