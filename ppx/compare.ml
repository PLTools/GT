open Base
open Ppxlib
open Printf
open Ast_helper
open Ppxlib.Ast_builder.Default
open GtHelpers

(* Compare plugin where we pass another value of the same type as 'inh
 * and return GT.comparison as 'syn
*)
(* let make_dest_param_names ?(loc=Location.none) ps =
 *   map_type_param_names ps ~f:(sprintf "%s_2")
 *
 * let hack_params ?(loc=Location.none) ps =
 *   let param_names = map_type_param_names ps ~f:id in
 *   let rez_names = make_dest_param_names ~loc ps in
 *   let name_migrations = List.zip_exn param_names rez_names in
 *   let assoc s =
 *     try List.Assoc.find_exn ~equal:String.equal name_migrations s
 *     with Caml.Not_found ->
 *       raise_errorf "can't find new typ for param `%s" s
 *   in
 *   let blownup_params =
 *     List.concat_map param_names
 *       ~f:(fun s1 ->
 *            [Typ.var ~loc s1; Typ.var ~loc @@ assoc s1 ]
 *          )
 *   in
 *   (param_names, rez_names, assoc, blownup_params) *)

open Plugin

let g initial_args = object(self: 'self)
  inherit ['self] Plugin.generator initial_args as super

  method plugin_name = "compare"

  method default_inh = core_type_of_type_declaration
  method syn_of_param ~loc s = [%type: GT.comparison]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: GT.comparison]

  method inh_of_param tdecl name =
    let loc = tdecl.ptype_loc in
    Typ.var ~loc name

  method plugin_class_params tdecl =
    List.map ~f:fst tdecl.ptype_params

  method prepare_inherit_args_for_alias ~loc tdecl rhs_args =
    rhs_args (* @ [ self#default_syn tdecl ] *)
               (* TODO: add 'extra ? *)

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

  method wrap_tr_function_str ~loc _tdelcl  make_gcata_of_class =
    let body = make_gcata_of_class [%expr self] in
    [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj]
(*
  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
    let names = make_new_names (List.length args) in
    let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
    let cmp_polyvar _ =
      assert false
    in
    let cmp_algebraic cname =
      Cf.method_concrete ~loc methname
      [%expr fun inh -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        Exp.match_ ~loc [%expr inh]
          [ case ~lhs ~guard:None ~rhs
          ; case ~lhs
          ]

      assert false
    in

    k @@
    match constr_info with
    | `Normal s -> cmp_algebraic cname
    | `Poly   s -> cmp_polyvar   cname

    k [
      (* TODO: inh syn stuff *)
        (* List.fold_left ~f:(fun acc (name,typ) ->
         *   [%expr [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ] [%e acc]
         *       [%e Exp.sprintf ~loc "%s" name]]
         * )
         * ~init:[%expr inh]
         * (List.zip_exn names args) *)
      ]]
  ]
*)

  method app_transformation_expr trf inh subj =
    let loc = trf.pexp_loc in
    [%expr [%e trf ] [%e inh] [%e subj]]

  method abstract_trf ~loc k =
    (* [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]] *)
    (* ignore inh attribute here too *)
    [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]]

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
    assert false

  method! on_variant tdecl ~mutal_names ~is_self_rec cds k =
    let loc = tdecl.ptype_loc in
    k @@
    List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple ts ->
          let names = List.map ts ~f:(fun _ -> gen_symbol ()) in
          let pat_names = List.map ts ~f:(fun _ -> gen_symbol ()) in
          let main_case =
            let lhs = Pat.construct ~loc (lident cd.pcd_name.txt) @@
              Some (Pat.tuple ~loc @@ List.map pat_names ~f:(Pat.var ~loc))
            in
            let rhs =
              List.fold_left  ~init:[%expr GT.EQ]
                (List.map3_exn pat_names names ts ~f:(fun a b c -> (a,b,c)))
                ~f:(fun acc (pname, name, typ) ->
                    [%expr GT.chain_compare [%e acc] (fun () ->
                        [%e
                           self#app_transformation_expr
                             (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                             (Exp.ident ~loc pname)
                             (Exp.ident ~loc name)
                        ])]
                  )

            in
            case ~lhs ~guard:None ~rhs
          in
          let other_case =
            let lhs  = Pat.var ~loc "other" in
            let rhs  =
              [%expr GT.compare_vari other
                  [%e
                    Exp.construct ~loc (lident cd.pcd_name.txt)@@
                    Some (Exp.tuple ~loc @@ List.map ts ~f:(fun _ -> [%expr Obj.magic ()]))
              ]]
            in
            case ~lhs ~guard:None ~rhs
          in
          Cf.method_concrete ~loc methname
            [%expr fun inh -> [%e

              Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
              Exp.match_ ~loc [%expr inh]
                [ main_case; other_case

                ]
            ]]

        | Pcstr_record ls ->
          assert false
          (* self#on_record_constr tdecl cd ls *)
      )

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    (* TODO: rewrite *)
    let ctuple =
      match bindings with
      | [] -> None
      | _  ->
        Some (Exp.tuple ~loc @@
              List.map bindings
                ~f:(fun (name, typ) ->
                    [%expr
                      [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
                      [%e einh]
                      [%e Exp.ident ~loc name ]
                    ]
                  )
             )
    in
    k @@ Exp.variant ~loc constr_name ctuple

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
