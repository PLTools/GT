open Base
open Ppxlib
open Printf
open Ast_helper
open GtHelpers
open Ppxlib.Ast_builder.Default


(* TODO: we want easily get foldr from foldl *)

let make_dest_param_names ?(loc=Location.none) ps =
  map_type_param_names ps ~f:(sprintf "%s_2")

let hack_params ?(loc=Location.none) ps =
  let param_names = map_type_param_names ps ~f:id in
  let rez_names = make_dest_param_names ~loc ps in
  let name_migrations = List.zip_exn param_names rez_names in
  let assoc s =
    try List.Assoc.find_exn ~equal:String.equal name_migrations s
    with Caml.Not_found ->
      raise_errorf "can't find new typ for param `%s" s
  in
  let blownup_params =
    List.concat_map param_names
      ~f:(fun s1 ->
           [Typ.var ~loc s1; Typ.var ~loc @@ assoc s1 ]
         )
  in
  (param_names, rez_names, assoc, blownup_params)

open Plugin

let g initial_args = object(self: 'self)
  inherit ['self] Plugin.generator initial_args as super

  method plugin_name = "foldl"

  method default_inh tdecl = self#default_syn tdecl
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: 'syn]

  method syn_of_param ~loc s = [%type: 'syn]
  method inh_of_param tdecl _name = self#default_syn tdecl

  method plugin_class_params tdecl =
    let loc = tdecl.ptype_loc in
    (* There we should have all parameters on the LHS of definition and 'syn one *)
    List.map ~f:fst tdecl.ptype_params @
    [self#default_syn tdecl; self#extra_param_stub ~loc]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    rhs_args @ [ self#default_syn tdecl ] @ [self#extra_param_stub ~loc]

  method! make_typ_of_self_trf ~loc tdecl =
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_typ_of_self_trf ~loc tdecl] ]

  method make_typ_of_class_argument ~loc tdecl name =
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_typ_of_class_argument ~loc tdecl name] ]

  method! make_RHS_typ_of_transformation ?subj_t ?syn_t tdecl =
    let loc = tdecl.ptype_loc in
    let subj_t = Option.value subj_t
        ~default:(using_type ~typename:tdecl.ptype_name.txt tdecl) in
    let syn_t  = Option.value syn_t  ~default:(self#default_syn tdecl) in
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_RHS_typ_of_transformation ~subj_t ~syn_t tdecl] ]

  (* method wrap_tr_function_typ (typ: core_type) =
   *   let loc = typ.ptyp_loc in
   *   typ
   *   [%type: [%t self#syn_of_param] -> [%t self#default_inh] -> [%t typ] ] *)

  method wrap_tr_function_str ~loc _tdelcl  make_gcata_of_class =
    let body = make_gcata_of_class [%expr self] in
    (* [%expr fun subj -> [%e expr] () subj] *)
    [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj]

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    k @@ Exp.variant ~loc constr_name @@
    Exp.maybe_tuple ~loc @@
    List.map bindings
      ~f:(fun (name, typ) ->
        [%expr
          [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
            [%e einh]
            [%e Exp.ident ~loc name ]
        ]
      )

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
    let names = make_new_names (List.length args) in
    let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
    k [
      (* TODO: inh syn stuff *)
    Cf.method_concrete ~loc methname
      [%expr fun inh -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        List.fold_left ~f:(fun acc (name,typ) ->
          [%expr [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ] [%e acc]
              [%e Exp.sprintf ~loc "%s" name]]
        )
        ~init:[%expr inh]
        (List.zip_exn names args)
      ]]
  ]

  method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    let pat = Pat.record ~loc ~flag:Closed @@
      List.map labs ~f:(fun l ->
          (Located.lident ~loc:l.pld_name.loc l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname
        [%expr fun () -> fun [%p pat ] -> [%e
          Exp.constant (const_string "asdf")
        ]]
    ]

end
