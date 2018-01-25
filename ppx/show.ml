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
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let plugin_name = "show"

let make_new_names n =
  List.init n ~f:(fun n ->  Char.to_string @@ Char.of_int_exn (n + Char.to_int 'a'))

let default_inh = let loc = Location.none in [%type: unit]
let default_syn = let loc = Location.none in [%type: string]

let make_class ~loc tdecl ~is_rec mutal_names =
  let cur_name = tdecl.ptype_name.txt in
  (* TODO: support is_rec to handle `type nonrec t = option t` *)

  let self_arg_name = "_fself" in
  let ans fields =
    (* inherit class_t and prepare to put other members *)
    let name = sprintf "%s_%s%s" plugin_name cur_name
        (match mutal_names with [] -> "" | _ -> "_stub")
    in
    Str.class_single ~loc
      ~name
      ~virt:Concrete
      ~params:tdecl.ptype_params
      ~wrap:(fun body ->
        (* constructor arguments are *)
        let names = List.map mutal_names ~f:(Pat.sprintf ~loc "%s_%s" plugin_name) @
                    [Pat.var ~loc self_arg_name] @
                    map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")
        in
        Cl.fun_list names body
      )
      @@
      [ let inh_params = prepare_param_triples ~loc
            ~inh:(fun ~loc _ -> default_inh)
            ~syn:(fun ~loc _ -> default_syn)
            (List.map ~f:fst tdecl.ptype_params)
        in
        Cf.inherit_ (Cl.constr (Located.lident ~loc ("class_"^cur_name)) inh_params)
      ] @ fields
  in

  let is_self_rec t = is_rec &&
    match t.ptyp_desc with
    | Ptyp_var _ -> false
    | Ptyp_constr ({txt=Lident s}, params)
      when String.equal s cur_name &&
           List.length params = List.length tdecl.ptype_params &&
           List.for_all2_exn params tdecl.ptype_params
             ~f:(fun a (b,_) -> 0=compare_core_type a b)
      -> is_rec
    | _ -> false
  in
  let rec do_typ ?with_arg t =
    let app_arg e =
      match with_arg with
      | Some x -> [e; Exp.ident x]
      | None -> [e]
    in
    if is_self_rec t then app_arg [%expr _fself ]
    else
      match t with
      | [%type: int]    -> app_arg [%expr fun () -> string_of_int ]
      | [%type: string] -> app_arg [%expr fun () x -> x ]
      | t -> match t.ptyp_desc with
        | Ptyp_var name ->
          app_arg Exp.(sprintf "f%s" name)
        | Ptyp_tuple params ->
          app_arg @@
          Exp.apply
            (Exp.sprintf "show_tuple%d" (List.length params))
            (List.concat_map params ~f:do_typ |> nolabelize)
        | Ptyp_constr (_,_) when is_self_rec t ->
          app_arg [%expr _fself ]
        | Ptyp_constr ({txt}, params) ->
          app_arg @@
          Exp.apply
            (Exp.ident_of_long @@ mknoloc @@
             map_longident ~f:((^)"show_") txt)
            (List.concat_map params ~f:do_typ |> nolabelize)
        | Ptyp_variant (rows, _, maybe_labels) -> begin
            let oninherit  = fun typs cident varname ->
                    Exp.apply_nolabeled ~loc
                      Exp.(ident_of_long ~loc @@ mknoloc @@
                           map_longident cident ~f:((^)"show_"))
                      ((List.concat_map typs ~f:(do_typ)) @
                       [[%expr ()]; Exp.ident ~loc varname])
            in
            match with_arg with
            | Some s ->
              prepare_patt_match_poly ~loc
                (Exp.sprintf ~loc "%s" s) rows maybe_labels
                ~onrow:(fun lab -> function
                    | [] -> Exp.constant @@ const_string ("`"^lab)
                    | args ->
                      let fmt = List.map args ~f:(fun _ -> "%a") in
                      let fmt = sprintf "`%s (%s)" lab (String.concat ~sep:"," fmt) in
                      Exp.apply_nolabeled ~loc
                        (Exp.apply1 ~loc [%expr Printf.sprintf]
                           (Exp.constant ~loc @@ const_string fmt))
                        (List.concat_map args ~f:(fun (name,t) -> do_typ ~with_arg:name t))
                )
                ~onlabel:(fun _ _ -> [%expr 1])
                ~oninherit
                :: []
            | None ->
              let k e = [%expr fun () foo -> [%e e]] :: [] in
              k @@ prepare_patt_match_poly ~loc
                (Exp.sprintf ~loc "foo") rows maybe_labels
                ~onrow:(fun lab -> function
                    | [] -> Exp.constant @@ const_string ("`"^lab)
                    | args ->
                      let fmt = List.map args ~f:(fun _ -> "%a") in
                      let fmt = sprintf "`%s (%s)" lab (String.concat ~sep:"," fmt) in

                      Exp.apply_nolabeled ~loc
                        (Exp.apply1 ~loc [%expr Printf.sprintf]
                           (Exp.constant ~loc @@ const_string fmt))
                        (List.concat_map args ~f:(fun (name,t) -> do_typ ~with_arg:name t))

                  )
                ~onlabel:(fun _ _ -> [%expr 1])
                ~oninherit
          end
        | _ -> failwith "Finish it!"
  in

  ans @@ visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let rec helper typ =
          match typ.ptyp_desc with
          | Ptyp_alias (t, aname) ->
            map_core_type t ~onvar:(fun as_ ->
              if String.equal as_ aname
              then Typ.constr (Located.lident ~loc tdecl.ptype_name.txt)@@
                List.map tdecl.ptype_params ~f:fst
              else Typ.var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
            let inh_params = params in
            let ans args =
              [ Cf.inherit_ ~loc @@ Cl.apply
                  (Cl.constr
                     ({cid with txt = map_longident cid.txt ~f:((^)"show_")})
                     inh_params)
                  (nolabelize args)
              ]
            in
            (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
            (* let self_arg = do_typ typ in *)
            ans @@ (Exp.sprintf ~loc "%s" self_arg_name) :: (List.concat_map params ~f:do_typ)
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
            (* todo: inherit something *)
            List.map rows ~f:(function
            | Rinherit typ ->
                with_constr_typ typ
                  ~fail:(fun () -> failwith "type is not a constructor")
                  ~ok:(fun cid params ->
                      let args = List.concat_map params ~f:do_typ in
                      let inh_params = params in
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
                  [%expr fun () -> [%e
                    Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
                    if List.length args = 0
                    then Exp.constant ~loc (Pconst_string ("`"^constr_name, None))
                    else
                      List.fold_left
                        (List.zip_exn names args)
                        ~f:(fun acc (name, typ) ->
                          Exp.apply_nolabeled ~loc acc (do_typ ~with_arg:name typ) )
                        ~init:[%expr Format.sprintf [%e
                            let fmt = String.concat ~sep:", " @@ List.map names
                                ~f:(fun _ -> "%a")
                            in
                            Exp.constant ~loc @@  const_string @@
                            sprintf "`%s(%s)" constr_name fmt
                      ]]

                  ]]

            )
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds ->
      List.map cds ~f:(fun cd ->
        let constr_name = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "wtf"
        | Pcstr_tuple ts ->
            let names = List.mapi ts
                ~f:(fun n _ -> Char.to_string @@ Char.of_int_exn
                       (n + Char.to_int 'a'))
            in
            Cf.method_concrete ~loc ("c_"^cd.pcd_name.txt)
              [%expr fun () -> [%e
                Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
                if List.length ts = 0
                then Exp.constant ~loc (Pconst_string (constr_name, None))
                else
                  List.fold_left
                    (List.zip_exn names ts)
                    ~f:(fun acc (name, typ) ->
                      Exp.apply_nolabeled ~loc acc (do_typ ~with_arg:name typ) )
                    ~init:[%expr Format.sprintf [%e
                        let fmt = String.concat ~sep:", " @@ List.map names
                            ~f:(fun _ -> "%a")
                        in
                        Exp.constant ~loc @@  const_string @@
                        sprintf "%s(%s)" constr_name fmt
                      ]]
              ]]
      )
    )


let make_trans_functions ~loc ~is_rec mutal_names tdecls =
  (* we will generate mutally recrsive showers here *)

  let make_class_name typname = sprintf "show_%s%s" typname
      (match mutal_names with [] -> "" | _ -> "_stub")
  in
  Str.value ~loc Recursive @@ List.map tdecls ~f:(fun tdecl ->
    let cur_name = tdecl.ptype_name.txt in
    let others =
      List.filter mutal_names ~f:(String.(<>) cur_name)
    in
    value_binding ~loc ~pat:(Pat.sprintf "show_%s" tdecl.ptype_name.txt)
      ~expr:(
        let arg_transfrs = map_type_param_names tdecl.ptype_params ~f:((^)"f") in
        let fixe = [%expr GT.fix0 ] in
        Exp.fun_list ~loc
          ~args:(List.map arg_transfrs ~f:(Pat.sprintf ~loc "%s"))
          [%expr fun () t -> [%e fixe] (fun self ->
            [%e Exp.apply1 ~loc (Exp.sprintf ~loc "gcata_%s" cur_name) @@
              Exp.apply ~loc (Exp.new_ ~loc @@ Located.lident ~loc @@
                              make_class_name cur_name) @@
              (nolabelize @@
               List.map others ~f:(Exp.sprintf ~loc "show_%s")
               @ [[%expr self] ]
               @ List.map arg_transfrs ~f:(Exp.sprintf ~loc "%s")
              )
            ]
          ) () t
          ]
      )
  )

let make_shortend_class ~loc ~is_rec mutal_names tdecls =
  List.map tdecls ~f:(fun tdecl ->
    let mutal_names = List.filter mutal_names ~f:(String.(<>) tdecl.ptype_name.txt) in
    let class_name = sprintf "show_%s" tdecl.ptype_name.txt in
    let stub_name = class_name ^ "_stub" in
    let mut_funcs = List.map ~f:((^)"show_") mutal_names in
    let real_args = "fself" :: (List.map ~f:((^)"f") @@ make_new_names (List.length tdecl.ptype_params)) in
    Str.single_class ~loc ~name:class_name
      ~wrap:(Cl.fun_list @@ List.map ~f:(Pat.sprintf ~loc "%s") @@ real_args)
      ~params:tdecl.ptype_params
      [ Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr ~loc (Located.lident ~loc stub_name) (List.map ~f:fst tdecl.ptype_params))
          (List.map ~f:(fun s -> Nolabel, Exp.sprintf ~loc "%s" s) @@ (mut_funcs@real_args))
      ]
  )

let do_single ~loc ~is_rec tdecl =
  [ make_class ~loc ~is_rec tdecl []
  ; make_trans_functions ~loc ~is_rec [] [tdecl]
  ]

let do_mutals ~loc ~is_rec tdecls =
  (* for mutal recursion we need to generate two classes and one function *)
  let mut_names = List.map tdecls ~f:(fun td -> td.ptype_name.txt) in
  List.map tdecls ~f:(fun tdecl ->
    make_class ~loc ~is_rec:true tdecl @@
      List.filter mut_names ~f:(String.(<>) tdecl.ptype_name.txt)
  ) @
  (make_trans_functions ~loc ~is_rec:true mut_names tdecls) ::
  (make_shortend_class  ~loc ~is_rec:true mut_names tdecls)
