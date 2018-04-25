(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
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


  (* TODO: next functions were required for previous implemntation.
   * Maybe I need to remove them and extra separate functions from
   ** plugin.ml *)

  (* method! make_RHS_typ_of_transformation ?subj_t ?syn_t tdecl =
   *   let loc = tdecl.ptype_loc in
   *   let subj_t = Option.value subj_t
   *       ~default:(using_type ~typename:tdecl.ptype_name.txt tdecl) in
   *   let syn_t  = Option.value syn_t ~default:(self#default_syn tdecl) in
   *   let r = super#make_RHS_typ_of_transformation ~syn_t ~subj_t tdecl in
   *   [%type: (string * [%t r ]) ] *)

  (* method! make_typ_of_class_argument ~loc tdecl name =
   *   [%type: (string * [%t super#make_typ_of_class_argument ~loc tdecl name ])] *)

  (* method! make_typ_of_self_trf ~loc tdecl =
   *   (\* If we change this in the manner similar to tranformations for arguments
   *      then we need to affect definition of transformation function (generate a
   *      string there) *\)
   *   super#make_typ_of_self_trf ~loc tdecl
   *   (\* [%type: (string * [%t super#make_typ_of_self_trf ~loc tdecl ])] *\) *)

    method! wrap_tr_function_str ~loc tdecl gcata_on_new_expr =
    (* TODO: pass tdecl here*)
    (* let str_e = self#string_of_typ ~loc (\* ~is_self_rec *\) @@
     *   using_type ~typename: tdecl.ptype_name.txt tdecl
     * in *)
    [%expr [%e super#wrap_tr_function_str ~loc tdecl gcata_on_new_expr ]]


  (* method! generate_for_variable ~loc name =
   *   [%expr snd [%e super#generate_for_variable ~loc name ]] *)

  (* method! extract_transformation ~loc etrf =
   *   [%expr snd [%e super#extract_transformation ~loc etrf ]] *)


  method! prepare_fa_args tdecl =
    let loc = tdecl.ptype_loc in
    List.concat @@ map_type_param_names tdecl.ptype_params
      ~f:(fun s -> [Pat.sprintf "typ_%s" s; Pat.sprintf ~loc "f%s" s])

  method apply_fas_in_new_object tdecl =
    let loc = tdecl.ptype_loc in
    List.concat @@
    map_type_param_names tdecl.ptype_params
      ~f:(fun s -> [Exp.sprintf ~loc "typ_%s" s; Exp.sprintf ~loc "f%s" s ])

  method! prepare_fa_arg_types tdecl =
    let loc = tdecl.ptype_loc in
    List.concat_map (super#prepare_fa_arg_types tdecl)
      ~f:(fun t -> [ [%type: string]; t])

  method private string_of_typ ~loc (* ~is_self_rec *) typ =
      let rec string_of_longident = function
      | Lident s -> s
      | Ldot (l, s) ->  string_of_longident l ^ "." ^ s
      | Lapply (_,_) -> assert false
      in

      let rec helper typ =
        match typ.ptyp_desc with
        | Ptyp_var s -> Exp.sprintf ~loc "typ_%s" s
        | Ptyp_constr ({txt}, []) ->
            Exp.constant ~loc @@ const_string @@ string_of_longident txt
        | Ptyp_constr ({txt},ts) ->
            List.map ~f:helper ts
            |> List.fold_right
              ~f:(fun e acc -> [%expr [%e e] ^ [%e acc]])
              ~init:(Exp.constant ~loc @@ const_string @@ string_of_longident txt)
        |  _ -> assert false
      in
      helper typ

  method make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams =
    let trfs = super#make_inherit_args_for_alias ~loc ~is_self_rec
        tdecl do_typ cid cparams
    in
    assert (List.length trfs = 1 + (List.length cparams));
    let trf_others = List.tl_exn trfs in
    (* TODO: fix hack *)
    let xs = List.map2_exn trf_others cparams
        ~f:(fun rez typ ->
          match typ.ptyp_desc with
          | Ptyp_var s -> [Exp.sprintf ~loc "typ_%s" s; Exp.sprintf ~loc "f%s" s ]
          | _ when is_self_rec typ ->
              [ [%expr "put_self typ here"]; [%expr fself ] ]
          | _ -> [ self#string_of_typ ~loc typ; rez]
        )
    in
    [%expr fself] :: (List.concat xs)

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

  method! compose_apply_transformations ~loc ~left right typ =
    let typ_str = [%expr "asdf" ] in
    Exp.apply_nolabeled left [ typ_str; right ]

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
