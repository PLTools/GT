(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib
open Printf
open Ast_helper
open GtHelpers
open Ppxlib.Ast_builder.Default

let param_name_mangler = sprintf "%s_2"

let hack_params ?(loc=Location.none) ps =
  let param_names = map_type_param_names ps ~f:id in
  let rez_names = map_type_param_names ps ~f:param_name_mangler in
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

let g args = object(self: 'self)
  inherit ['self] Plugin.generator args

  method plugin_name = "gmap"

  method default_inh = let loc = Location.none in [%type: unit]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    let param_names,rez_names,find_param,blownup_params =
      hack_params tdecl.ptype_params
    in
    let ans =
      let cur_name = self#cur_name tdecl in
      let (ident,args) =
          (cur_name, rez_names)
      in
      Typ.constr ~loc (Located.lident ~loc ident) @@
      List.map ~f:(Typ.var ~loc) args
    in
    if is_polyvariant_tdecl tdecl
    then
      Typ.alias ~loc (openize_poly ans) "extra"
      (* [%type: ([> [%t ans]] as 'extra)] *)
    else ans

  method syn_of_param ~loc s = Typ.var ~loc @@ param_name_mangler s
  method plugin_class_params tdecl =
    let loc = tdecl.ptype_loc in
    let param_names,_,find_param,blownup_params = hack_params tdecl.ptype_params in
    blownup_params @ [self#extra_param_stub ~loc]

  method prepare_inherit_args_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      hack_params tdecl.ptype_params
    in
    List.concat_map rhs_args ~f:(fun t ->
      [t; map_core_type t ~onvar:(fun s -> Typ.var ~loc (find_param s))]
    ) @ [self#extra_param_stub ~loc]

  method! extra_class_sig_members tdecl =
    let loc = tdecl.ptype_loc in
    if is_polyvariant_tdecl tdecl
    then
      let _param_names,rez_names,_find_param,_blownup_params =
        hack_params tdecl.ptype_params
      in
      let right = Typ.constr ~loc
          (Located.map (fun s -> Lident s) tdecl.ptype_name)
          (List.map ~f:(Typ.var ~loc) rez_names)
      in
      let right = openize_poly right in
      [Ctf.constraint_ ~loc (self#extra_param_stub ~loc) right ]
    else []

  method! extra_class_str_members tdecl =
    let loc = tdecl.ptype_loc in
    if is_polyvariant_tdecl tdecl
    then
      let _param_names,rez_names,_find_param,_blownup_params =
        hack_params tdecl.ptype_params
      in
      let right = Typ.constr ~loc
          (Located.map (fun s -> Lident s) tdecl.ptype_name)
          (List.map ~f:(Typ.var ~loc) rez_names)
      in
      let right = openize_poly right in
      [Cf.constraint_ ~loc (self#extra_param_stub ~loc) right ]
    else []

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings  einh k =
    let ctuple =
      match bindings with
      | [] -> None
      | _  ->
        Some (Exp.tuple ~loc @@
              List.map bindings
                ~f:(fun (name, typ) ->
                    [%expr
                      [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
                      [%e Exp.ident ~loc name ]
                    ]
                  )
             )
    in
    k @@ Exp.variant ~loc constr_name ctuple

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info ts k =
    let names = make_new_names (List.length ts) in
    let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
    k [
    Cf.method_concrete ~loc methname
      [%expr fun () -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        let ctuple =
          if List.length ts = 0
          then None
          else Some (Exp.tuple ~loc @@
                     List.map (List.zip_exn names ts)
                       ~f:(fun (name, typ) ->
                           self#app_transformation_expr
                             (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                             [%expr assert false]
                             (Exp.ident ~loc name)
                         )
                    )
        in
        (match constr_info with `Normal s -> Exp.construct ~loc (lident s)
                              | `Poly s   -> Exp.variant ~loc s
        )
          ctuple
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
        [%expr fun () -> [%e
          Exp.fun_ ~loc Nolabel None pat @@
          Exp.record ~loc @@ List.map labs
            ~f:(fun {pld_name; pld_type} ->
                lident pld_name.txt,
                self#app_transformation_expr
                  (self#do_typ_gen ~loc ~is_self_rec ~mutal_names pld_type)
                  [%expr assert false]
                  (Exp.ident ~loc pld_name.txt)

              )
        ]]
    ]

end
