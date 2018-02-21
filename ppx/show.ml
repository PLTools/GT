(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core
open Printf
open Ast_helper
open GtHelpers
open Ppx_core.Ast_builder.Default

let g = object(self: 'self)
  inherit ['self] Plugin.generator

  method plugin_name = "show"
  method default_inh = let loc = Location.none in [%type: unit]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: string]

  method syn_of_param ~loc _ = [%type: string]

  method plugin_class_params tdecl = List.map ~f:fst tdecl.ptype_params

  method got_constr ~loc tdecl is_self_rec do_typ cid cparams k =
    let ans args =
      [ Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr
             ({cid with txt = map_longident cid.txt
                            ~f:(sprintf "%s_%s" self#plugin_name)})
             cparams)
          (nolabelize args)
      ]
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    k @@ ans @@
    (Exp.sprintf ~loc "%s" Plugin.self_arg_name) ::
    (List.map cparams ~f:(do_typ ~loc is_self_rec))


  method generate_for_polyvar_tag ~loc constr_name bindings is_self_rec einh k =
    match bindings with
    | [] -> k @@ Exp.constant ~loc (Pconst_string ("`"^constr_name, None))
    | _ ->
      k @@ List.fold_left
        bindings
        ~f:(fun acc (name, typ) -> Exp.apply1 ~loc acc
               [%expr
                 [%e self#do_typ_gen ~loc is_self_rec typ]
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


  method got_polyvar ~loc tdecl do_typ is_self_rec rows k =
    k @@
    List.map rows ~f:(function
        | Rinherit typ ->
          with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
                let args = List.map params ~f:(self#do_typ_gen ~loc is_self_rec) in
                (* gmap has blownup_params here. Maybe we should abstract this *)
                let inh_params = params @ [[%type: 'polyvar_extra]] in
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
              self#generate_for_polyvar_tag ~loc constr_name (List.zip_exn names args)
                is_self_rec [%expr inh] (fun x -> x)

            ]]

      )


  method on_tuple_constr tdecl is_self_rec cd ts =
    let loc = tdecl.ptype_loc in
    let names = make_new_names (List.length ts) in
    let constr_name = cd.pcd_name.txt in
    Cf.method_concrete ~loc ("c_"^cd.pcd_name.txt)
      [%expr fun () -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        if List.length ts = 0
        then Exp.constant ~loc (Pconst_string (constr_name, None))
        else
          List.fold_left
            (List.zip_exn names ts)
            ~f:(fun acc (name, typ) ->
                Exp.apply1 ~loc acc
                  [%expr
                    [%e self#do_typ_gen ~loc  is_self_rec typ ]
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
