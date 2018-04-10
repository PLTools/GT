(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Base
open Ppxlib
open Printf
open Ast_helper
open GtHelpers
open Ppxlib.Ast_builder.Default

class ['self] g args = object(self: 'self)
  inherit ['self] Plugin.generator args

  method plugin_name = "show"
  method default_inh = let loc = Location.none in [%type: unit]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: string]

  method syn_of_param ~loc _ = [%type: string]

  method plugin_class_params tdecl =
    let loc = tdecl.ptype_loc in
    (List.map ~f:fst tdecl.ptype_params) @ [self#extra_param_stub ~loc]

  method prepare_inherit_args_for_alias ~loc tdecl rhs_args =
    rhs_args @ [self#extra_param_stub ~loc]

  (* We are constrainting extra type parameter using separate member of the class.
   * The alternative will be to use `()` everywhere instead of `inh` identifier *)
  method! extra_class_str_members tdecl =
    let loc = tdecl.ptype_loc in
    [ Cf.constraint_  ~loc [%type: 'inh] [%type: unit] ]

  method! extra_class_sig_members tdecl =
    let loc = tdecl.ptype_loc in
    [ Ctf.constraint_  ~loc [%type: 'inh] [%type: unit] ]


  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    match bindings with
    | [] -> k @@ Exp.constant ~loc (Pconst_string ("`"^constr_name, None))
    | _ ->
      k @@ List.fold_left
        bindings
        ~f:(fun acc (name, typ) -> Exp.apply1 ~loc acc
               [%expr
                 [%e self#do_typ_gen ~loc ~mutal_names ~is_self_rec typ]
                 [%e Exp.ident ~loc name ]
               ])
        ~init:[%expr Format.sprintf [%e
            let fmt = String.concat ~sep:", " @@ List.map bindings
                ~f:(fun _ -> "%s")
            in
            Exp.constant ~loc @@ const_string @@
            sprintf "`%s(%s)" constr_name fmt
          ]]


  (* (\* this is the same for show and gmap *\)
   * method got_polyvar ~loc ~is_self_rec ~mutal_names tdecl do_typ  rows k =
   *   k @@
   *   List.map rows ~f:(function
   *       | Rinherit typ ->
   *         with_constr_typ typ
   *           ~fail:(fun () -> failwith "type is not a constructor")
   *           ~ok:(fun cid params ->
   *             let args = List.map params
   *                 ~f:(self#do_typ_gen ~loc ~mutal_names ~is_self_rec) in
   *             (\* gmap has blownup_params here. Maybe we should abstract this *\)
   *             let inh_params = self#prepare_inherit_args_for_alias ~loc
   *                 tdecl params
   *             in
   *
   *             Cf.inherit_ ~loc @@ Cl.apply
   *                 (Cl.constr ~loc
   *                    (map_longident cid.txt ~f:(sprintf "show_%s"))
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
   *             self#generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
   *               constr_name (List.zip_exn names args)
   *               [%expr inh] (fun x -> x)
   *
   *           ]]
   *
   *     ) *)

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info ts k =
    k @@
    [ let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
      let string_of_name = match constr_info with
        | `Poly s -> sprintf "`%s" s
        | `Normal s -> s
      in
      Cf.method_concrete ~loc methname
      [%expr fun () -> [%e
        let names = make_new_names (List.length ts) in
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        if List.length ts = 0
        then Exp.constant ~loc (const_string string_of_name)
        else
          List.fold_left
            (List.zip_exn names ts)
            ~f:(fun acc (name, typ) ->
                Exp.apply1 ~loc acc
                  (self#app_transformation_expr
                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                     [%expr assert false]
                     (Exp.ident ~loc name)
                  )
              )
            ~init:[%expr Format.sprintf [%e
                let fmt = String.concat ~sep:", " @@ List.map names
                    ~f:(fun _ -> "%s")
                in
                Exp.constant ~loc @@  const_string @@
                sprintf "%s(%s)" string_of_name fmt
              ]]
      ]]
  ]

end

let g = new g
