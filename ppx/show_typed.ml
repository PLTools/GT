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
    let syn_t  = Option.value syn_t  ~default:(self#default_syn tdecl) in
    let r = super#make_RHS_typ_of_transformation ~syn_t ~subj_t tdecl in
    [%type: (string * [%t r ]) ]

  method! generate_for_variable ~loc name =
    [%expr snd [%e super#generate_for_variable ~loc name ]]

  (* method! make_trans_function_body ~loc ?(rec_typenames=[]) class_name tdecl =
   *   Exp.tuple ~loc
   *     [ Exp.constant ~loc @@ const_string "asdf"
   *     ; super#make_trans_function_body ~loc ~rec_typenames class_name tdecl
   *     ] *)

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


  (* this is the same for show and gmap *)
  method got_polyvar ~loc ~is_self_rec ~mutal_names tdecl do_typ rows k =
    k @@
    List.map rows ~f:(function
        | Rinherit typ ->
          with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
              let args = List.map params
                  ~f:(self#do_typ_gen ~loc ~is_self_rec ~mutal_names) in
                (* gmap has blownup_params here. Maybe we should abstract this *)
                let inh_params = self#prepare_inherit_args_for_alias ~loc
                    tdecl params
                in

                Cf.inherit_ ~loc @@ Cl.apply
                  (Cl.constr
                     ({cid with txt = map_longident cid.txt ~f:((^)"show_")})
                     inh_params
                  )
                  (nolabelize ([%expr _fself]::args))
              )
        | Rtag (constr_name,_,_,args) ->
          let names = make_new_names (List.length args) in

          Cf.method_concrete ~loc ("c_" ^ constr_name)
            [%expr fun inh -> [%e
              Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
              self#generate_for_polyvar_tag ~loc
                constr_name (List.zip_exn names args)
                ~is_self_rec ~mutal_names [%expr inh] (fun x -> x)

            ]]

      )

  method on_tuple_constr ~is_self_rec ~mutal_names tdecl  cd ts =
    let loc = tdecl.ptype_loc in
    let constr_name = cd.pcd_name.txt in
    Cf.method_concrete ~loc ("c_"^constr_name)
      [%expr fun () -> [%e
        let names = make_new_names (List.length ts) in
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        if List.length ts = 0
        then Exp.constant ~loc (Pconst_string (constr_name, None))
        else
          List.fold_left
            (List.zip_exn names ts)
            ~f:(fun acc (name, typ) ->
                Exp.apply1 ~loc acc
                  [%expr
                    [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
                    ()
                    [%e Exp.ident ~loc name]
                  ]
              )
            ~init:[%expr Format.sprintf [%e
                let fmt = String.concat ~sep:", " @@ List.map names
                    ~f:(fun _ -> "%s")
                in
                Exp.constant ~loc @@  const_string @@
                sprintf "%s(%s)" constr_name fmt
              ]]
      ]]


end
