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
          Exp.constant (const_string "asdf")
        ]]
    ]

end

let g = new g
