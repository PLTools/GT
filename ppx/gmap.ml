(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *)

open Ppx_core
open Printf
open Ast_helper
open GtHelpers
open Ppx_core.Ast_builder.Default

let param_name_mangler = sprintf "%s_2"

let hack_params ?(loc=Location.none) ps =
  let param_names = map_type_param_names ps ~f:id in
  let rez_names = map_type_param_names ps ~f:param_name_mangler in
  let name_migrations = List.zip_exn param_names rez_names in
  let assoc s =
    try List.Assoc.find_exn ~equal:String.equal name_migrations s
    with Not_found ->
      raise_errorf "can't find new typ for param `%s" s
  in
  let blownup_params =
    List.concat_map param_names
      ~f:(fun s1 ->
           [Typ.var ~loc s1; Typ.var ~loc @@ assoc s1 ]
         )
  in
  (param_names, rez_names, assoc, blownup_params)

let g = object(self: 'self)
  inherit ['self] Plugin.generator

  method plugin_name = "gmap"

  method default_inh = let loc = Location.none in [%type: unit]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    let cur_name = self#cur_name tdecl in
    let param_names,rez_names,find_param,blownup_params = hack_params tdecl.ptype_params in
    let (ident,args) =
      if is_polyvariant_tdecl tdecl
      then
        (cur_name^"_open", "polyvar_extra"::rez_names)
      else
        (cur_name, rez_names)

    in
    Typ.constr ~loc (Located.lident ~loc ident) @@
    List.map ~f:(Typ.var ~loc) args

  method syn_of_param ~loc s = Typ.var ~loc @@ param_name_mangler s
  method plugin_class_params tdecl =
    let param_names,_,find_param,blownup_params = hack_params tdecl.ptype_params in
    blownup_params

  method prepare_inherit_args_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      hack_params tdecl.ptype_params
    in
    List.concat_map rhs_args ~f:(fun t ->
      [t; map_core_type t ~onvar:(fun s -> Typ.var ~loc (find_param s))]
    )

  method got_constr ~loc tdecl is_self_rec do_typ cid cparams k =
    (* TODO: abstract out and override only arguments of inherited class *)
    let ans2 args =
      [ let params = self#prepare_inherit_args_for_alias ~loc tdecl cparams in
          (* List.concat_map cparams ~f:(fun t ->
           *   [t; map_core_type t ~onvar:(fun s -> Typ.var ~loc (find_param s))]
           * ) in *)

        Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr (Located.map (map_longident ~f:((^)"gmap_")) cid)
             params)
          (nolabelize args)
      ]
    in
    (* for typ aliases we can cheat because first argument of constructor of type
       on rhs is self transformer function *)
    (* let self_arg = do_typ typ in *)
    k @@ ans2 @@
    (Exp.sprintf ~loc "%s" Plugin.self_arg_name) ::
    (List.map cparams ~f:(self#do_typ_gen ~loc is_self_rec))

  method generate_for_polyvar_tag ~loc constr_name bindings is_self_rec einh k =
    let ctuple =
      match bindings with
      | [] -> None
      | _  ->
        Some (Exp.tuple ~loc @@
              List.map bindings
                ~f:(fun (name, typ) ->
                    [%expr
                      [%e self#do_typ_gen ~loc is_self_rec typ ]
                      [%e einh]
                      [%e Exp.ident ~loc name ]
                    ]
                  )
             )
    in
    k @@ Exp.variant ~loc constr_name ctuple


  method got_polyvar ~loc tdecl do_typ is_self_rec rows k =
    let _param_names,_rez_names,_find_param,blownup_params =
      hack_params tdecl.ptype_params
    in
    k @@
    List.map rows ~f:(function
        | Rinherit typ ->
          with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
                let args = List.map params ~f:(self#do_typ_gen ~loc is_self_rec) in
                let inh_params = blownup_params @ [[%type: 'polyvar_extra]] in
                Cf.inherit_ ~loc @@ Cl.apply
                  (Cl.constr
                     ({cid with txt = map_longident cid.txt ~f:((^)"gmap_")})
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
                is_self_rec [%expr (inh: unit)] (fun x -> x)
            ]]

      )


  method on_tuple_constr tdecl is_self_rec cd ts =
    let loc = tdecl.ptype_loc in
    let names = make_new_names (List.length ts) in
    let constr_name = cd.pcd_name.txt in
    Cf.method_concrete ~loc ("c_"^cd.pcd_name.txt)
      [%expr fun () -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        let ctuple =
          if List.length ts = 0
          then None
          else Some (Exp.tuple ~loc @@
                     List.map (List.zip_exn names ts)
                       ~f:(fun (name, typ) ->
                             [%expr
                               [%e self#do_typ_gen ~loc  is_self_rec typ ]
                               ()
                               [%e Exp.ident ~loc name]
                             ]
                         )
                    )
        in
        Exp.construct ~loc (Located.mk ~loc (Lident constr_name)) ctuple
      ]]

end
