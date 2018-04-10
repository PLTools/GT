(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Base
open Ppxlib
open Printf
open Ast_helper
open GtHelpers
open Ppxlib.Ast_builder.Default

let g args = object(self: 'self)
  inherit ['self] Show.g args as super

  method plugin_name = "show_typed"

  (* method! make_trans_function_typ tdecl =
   *   let loc = tdecl.ptype_loc in
   *   [%type: (string * [%t super#make_trans_function_typ tdecl]) ] *)

  method! make_RHS_typ_of_transformation ?subj_t ?syn_t tdecl =
    let loc = tdecl.ptype_loc in
    let subj_t = Option.value subj_t
        ~default:(using_type ~typename:tdecl.ptype_name.txt tdecl) in
    let syn_t  = Option.value syn_t ~default:(self#default_syn tdecl) in
    let r = super#make_RHS_typ_of_transformation ~syn_t ~subj_t tdecl in
    [%type: (string * [%t r ]) ]

  method! make_typ_of_class_argument ~loc name =
    [%type: (string * [%t super#make_typ_of_class_argument ~loc name ])]

  method! make_typ_of_self_trf ~loc tdecl =
    [%type: (string * [%t super#make_typ_of_self_trf ~loc tdecl ])]

  method! wrap_tr_function_str ~loc gcata_on_new_expr =
    (* TODO: pass tdecl here*)
    [%expr ("wtf", [%e super#wrap_tr_function_str ~loc gcata_on_new_expr ])]

  method! generate_for_variable ~loc name =
    [%expr snd [%e super#generate_for_variable ~loc name ]]

  method make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams =
    let trfs = super#make_inherit_args_for_alias ~loc ~is_self_rec
        tdecl do_typ cid cparams
    in
    assert (List.length trfs = 1 + (List.length cparams));
    (* basically we need to convert every expression to a pair where fst is a string*)

    let f e = [%expr ("asdf", [%e e])] in
    List.map ~f trfs

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings  einh k =
    match bindings with
    | [] -> k @@ Exp.constant ~loc (Pconst_string ("`"^constr_name, None))
    | _ ->
      k @@ List.fold_left
        bindings
        ~f:(fun acc (name, typ) -> Exp.apply1 ~loc acc
               [%expr
                 [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ]
                 ([%e einh ]: unit)
                 [%e Exp.ident ~loc name ]
               ])
        ~init:[%expr Format.sprintf [%e
            let fmt = String.concat ~sep:", " @@ List.map bindings
                ~f:(fun _ -> "%s")
            in
            Exp.constant ~loc @@ const_string @@
            sprintf "`%s(%s)" constr_name fmt
          ]]


  (* method got_polyvar ~loc ~is_self_rec ~mutal_names tdecl do_typ rows k =
   *   k @@
   *   List.map rows ~f:(function
   *       | Rinherit typ ->
   *         with_constr_typ typ
   *           ~fail:(fun () -> failwith "type is not a constructor")
   *           ~ok:(fun cid params ->
   *             let args = List.map params
   *                 ~f:(self#do_typ_gen ~loc ~is_self_rec ~mutal_names) in
   *               (\* gmap has blownup_params here. Maybe we should abstract this *\)
   *               let inh_params = self#prepare_inherit_args_for_alias ~loc
   *                   tdecl params
   *               in
   *
   *               Cf.inherit_ ~loc @@ Cl.apply
   *                 (Cl.constr
   *                    (map_longident cid.txt ~f:(sprintf "%s_%s" self#plugin_name))
   *                    inh_params
   *                 )
   *                 (\* TODO: maybe we should augment arguemnts here *\)
   *                 (nolabelize ((Exp.sprintf ~loc "%s" Plugin.self_arg_name)::args))
   *             )
   *       | Rtag (constr_name,_,_,args) ->
   *         let names = make_new_names (List.length args) in
   *
   *         Cf.method_concrete ~loc ("c_" ^ constr_name)
   *           [%expr fun inh -> [%e
   *             Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
   *             self#generate_for_polyvar_tag ~loc
   *               constr_name (List.zip_exn names args)
   *               ~is_self_rec ~mutal_names [%expr inh] (fun x -> x)
   *
   *           ]]
   *
   *     ) *)



  (* is the same as for base class *)
  (* method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info ts k =
   *   let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
   *   let string_of_name = match constr_info with
   *     | `Poly s -> sprintf "`%s" s
   *     | `Normal s -> s
   *   in
   *
   *   k @@
   *   [ Cf.method_concrete ~loc methname
   *     [%expr fun () -> [%e
   *       let names = make_new_names (List.length ts) in
   *       Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
   *       if List.length ts = 0
   *       then Exp.constant ~loc (const_string string_of_name)
   *       else
   *         List.fold_left
   *           (List.zip_exn names ts)
   *           ~f:(fun acc (name, typ) ->
   *               Exp.apply1 ~loc acc
   *                 (self#app_transformation_expr
   *                    (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
   *                    [%expr assert false]
   *                    (Exp.ident ~loc name)
   *                 )
   *             )
   *           ~init:[%expr Format.sprintf [%e
   *               let fmt = String.concat ~sep:", " @@ List.map names
   *                   ~f:(fun _ -> "%s")
   *               in
   *               Exp.constant ~loc @@  const_string @@
   *               sprintf "%s(%s)" string_of_name fmt
   *             ]]
   *     ]]
   *   ] *)

end
