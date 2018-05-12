(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Base
open Ppxlib
open Ppxlib.Ast_builder.Default
open HelpersBase
open Printf
let (@@) = Caml.(@@)

let class_name_for_typ name = sprintf "%s_t" name
let trait_class_name_for_typ ~trait name =
  class_name_for_typ (if String.equal trait ""
                      then name
                      else sprintf "%s_%s" trait name)

type config_plugin = Skip | Use of Plugin_intf.plugin_args

module Make(AstHelpers : GTHELPERS_sig.S) = struct

open AstHelpers

let prepare_patt_match ~loc what constructors make_rhs =
  let on_alg cdts =
    let k cs = Exp.match_ ~loc what cs in
    k @@ List.map cdts ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "wtf"
        | Pcstr_tuple args ->
            let names = List.map args ~f:(fun _ -> gen_symbol ()) in
            case
              ~lhs:(Pat.constr ~loc cd.pcd_name.txt @@
                    List.map ~f:(Pat.var ~loc) names
                   )
              ~rhs:(make_rhs cd names)
      )
  in
  let on_poly cs =
    assert false
  in
  match constructors with
  | `Algebraic cdts -> on_alg cdts
  | `PolyVar cs -> on_poly cs

let prepare_patt_match_poly ~(loc:loc) what rows labels ~onrow ~onlabel ~oninherit =
  let k cs = Exp.match_ ~loc what cs in
  let rs =
    List.map rows ~f:(function
        | Rtag (lab, _, _, args) ->
          let args = match args with
            | [t] -> unfold_tuple t
            | [] -> []
            | _ -> failwith "we don't support conjunction types"
          in
          let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
          let lhs = Pat.variant ~loc  lab @@
            List.map ~f:(fun s -> Pat.var ~loc (Located.mk ~loc s)) names
          in
          case ~lhs
            ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;_},ts) ->
            let newname = "subj" in
            let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc txt))
                (Located.mk ~loc newname)
            in
            case ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let ls = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc (Lident lab)) )
            (Located.mk ~loc newname)
        in
        case ~lhs ~rhs:(onlabel lab newname)
      )
  in
  k @@ rs@ls

let inh_syn_ts ~loc = [ Typ.var ~loc "inh"; Typ.var ~loc "syn" ]

let params_of_interface_class ~loc tdecl =
  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (for polyvar subtyping)
  *)
  (List.concat @@ map_type_param_names tdecl.ptype_params
     ~f:(fun s ->
         [ named_type_arg ~loc s
         ; named_type_arg ~loc ("i"^s)
         ; named_type_arg ~loc ("s"^s) ]
       )
  )
  @ [ named_type_arg ~loc "inh"
    ; named_type_arg ~loc "syn"
    ; named_type_arg ~loc Plugin.extra_param_name]

let make_interface_class_sig ~loc tdecl =
  let params = List.map ~f:fst tdecl.ptype_params in
  let name = tdecl.ptype_name in

  let k fields =
    Sig.class_ ~loc ~virt:false
      ~name:(class_name_for_typ name.txt)
      ~params:(params_of_interface_class ~loc tdecl)
      fields
  in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _labels ->
        k []
      )
    ~onvariant:(fun cds ->
      k @@
      List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "record constructors"
        | Pcstr_tuple ts ->
          (* Abstract out the type below  *)
            Ctf.method_ ~loc methname
              (List.fold_right ts ~init:(Typ.var ~loc "syn")
                 ~f:(fun t -> Typ.arrow ~loc (Typ.from_caml t))
                |> (Typ.arrow ~loc (Typ.var ~loc "inh"))
               )
      )
    )
    ~onmanifest:(fun typ ->
          let wrap name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> ptyp_var typ.ptyp_loc ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> ptyp_var typ.ptyp_loc ("s"^n) )
                  ]
                )
              |> List.map ~f:Typ.from_caml
            in
            let inh_params = List.concat
                [ inh_params
                ; inh_syn_ts ~loc
                ; [ Typ.var ~loc Plugin.extra_param_name ]
                ]
            in

            [ Ctf.inherit_ ~loc @@
              Cty.constr ~loc (map_longident ~f:(sprintf "class_%s") name)
                inh_params
            ]
          in

          let rec helper typ = match typ with
          | _ -> match typ.ptyp_desc with
          | Ptyp_var name -> not_implemented "antiphantom types"
          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            k @@ wrap txt params
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts) in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
            let loc = typ.ptyp_loc in
            map_core_type typ ~onvar:(fun as_ ->
                let open Ppxlib.Ast_builder.Default in
                if String.equal as_ new_name
                then ptyp_constr ~loc
                    (Located.lident ~loc name.txt) params
                else ptyp_var ~loc as_
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
              let meths =
                List.concat_map rows ~f:(function
                | Rtag (lab,_,_,args)  ->
                  let methname = sprintf "c_%s" lab in
                  [ Ctf.method_ ~loc methname @@
                      (List.fold_right args ~init:(Typ.var ~loc "syn")
                         ~f:(fun t -> Typ.arrow ~loc (Typ.from_caml t))
                       |> (Typ.arrow ~loc (Typ.var ~loc "inh"))
                      )
                  ]
                | Rinherit typ -> match typ.ptyp_desc with
                  | Ptyp_constr ({txt;loc}, params) ->
                     wrap txt params
                  | _ -> assert false
                )
              in
              k meths
          | _ -> failwith "not implemented"
          in
          helper typ
    )

let make_interface_class ~loc tdecl =
  let params = List.map ~f:fst tdecl.ptype_params in
  let name = tdecl.ptype_name in

  let ans ?(is_poly=false) fields =
    Str.class_single ~loc ~name:(class_name_for_typ name.txt) fields
      ~virt:true
      ~params:(params_of_interface_class ~loc tdecl)
  in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _ ->
        ans
          [ Cf.method_virtual ~loc (sprintf "do_%s" tdecl.ptype_name.txt) @@
            Typ.(arrow ~loc (var ~loc "inh") @@
                 arrow ~loc (use_tdecl tdecl)
                   (var ~loc "syn")
                )
          ]
      )
    ~onvariant:(fun cds ->
      ans @@
      List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "record constructors"
        | Pcstr_tuple ts ->
            Cf.method_virtual ~loc methname @@
            (List.fold_right ts ~init:Typ.(var ~loc "syn")
               ~f:(fun t -> Typ.arrow ~loc (Typ.from_caml t))
             |> Typ.(arrow ~loc (var ~loc "inh") )
            )
      )
    )
    ~onmanifest:(fun typ ->
          let wrap ?(is_poly=false) name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> ptyp_var typ.ptyp_loc ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> ptyp_var typ.ptyp_loc ("s"^n) )
                  ]
                )
              |> List.map ~f:Typ.from_caml
            in
            let inh_params = List.concat
                [ inh_params
                ; inh_syn_ts ~loc
                ; [ Typ.var ~loc Plugin.extra_param_name ]
                ]
            in

            [ Cf.inherit_ ~loc @@
              Cl.constr ~loc (map_longident ~f:(sprintf "class_%s") name)
                inh_params
            ]
          in

          let rec helper typ = match typ with
          | _ -> match typ.ptyp_desc with
          | Ptyp_var name -> not_implemented "antiphantom types"
          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            ans @@ wrap txt params
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts)
            in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
            let loc = typ.ptyp_loc in
            map_core_type typ ~onvar:(fun as_ ->
                let open Ppxlib.Ast_builder.Default in
                if String.equal as_ new_name
                then ptyp_constr ~loc
                    (Located.lident ~loc name.txt) params
                else ptyp_var ~loc as_
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
              ans ~is_poly:true @@
              List.concat_map rows ~f:(function
                | Rtag (lab,_,_,[]) ->
                    let methname = sprintf "c_%s" lab in
                    [ Cf.method_virtual ~loc methname @@
                      Typ.(arrow ~loc (var ~loc "inh") (var ~loc "syn"))
                    ]
                | Rtag (lab,_,_,[typ]) ->
                      (* print_endline "HERE"; *)
                      let args = match typ.ptyp_desc with
                        | Ptyp_tuple ts -> ts
                        | _ -> [typ]
                      in
                      let methname = sprintf "c_%s" lab in
                      [ Cf.method_virtual ~loc methname @@
                          (List.fold_right args ~init:(Typ.var ~loc "syn")
                             ~f:(fun t -> Typ.arrow ~loc (Typ.from_caml t))
                           |> (Typ.arrow ~loc (Typ.var ~loc "inh"))
                          )
                      ]
                    | Rtag (_,_,_,_args) ->
                      failwith "Can't deal with conjunctive types"

                    | Rinherit typ -> match typ.ptyp_desc with
                      | Ptyp_constr ({txt;loc}, params) ->
                        wrap ~is_poly:true txt params
                      | _ -> assert false
                  )
          | _ -> failwith "not implemented"
          in
          helper typ
    )

let make_gcata_typ ~loc tdecl =
  let tr_t = visit_typedecl ~loc tdecl
      ~onrecord:(fun _ ->
          Typ.object_ ~loc Open @@
          [ sprintf "do_%s" tdecl.ptype_name.txt,
            Typ.(chain_arrow ~loc
              [var ~loc "inh"; use_tdecl tdecl; var ~loc "syn"]
                )
          ]
        )
      ~onvariant:(fun cds ->
          Typ.object_ ~loc Open @@
          List.map cds
            ~f:(fun cd -> match cd.pcd_args with
                | Pcstr_tuple ts ->
                  let new_ts = (List.map ts ~f:(Typ.from_caml)) @ [Typ.var ~loc "syn"] in
                  let new_ts = Typ.var ~loc "inh" :: new_ts in
                  (sprintf "c_%s" cd.pcd_name.txt, Typ.chain_arrow ~loc new_ts)
              | Pcstr_record _ -> assert false
            )
        )
      ~onmanifest:(fun typ ->
          match typ.ptyp_desc with
          | Ptyp_constr ({txt;_}, ts) ->
            (* there we can fuck up extra argument about polymorphic variants *)
            let args = map_type_param_names tdecl.ptype_params ~f:(fun name ->
                [ Typ.var ~loc name
                ; Typ.any ~loc
                ; Typ.var ~loc @@ "s"^name ]
              ) |> List.concat
            in
            let args = args @ [ Typ.var ~loc "inh"; Typ.var ~loc "syn"; Typ.any ~loc ] in
            Typ.class_ ~loc (Lident(sprintf "class_%s" tdecl.ptype_name.txt)) args
          | Ptyp_variant (rows,_flg,_) ->
              let params = map_type_param_names tdecl.ptype_params
                  ~f:(fun s ->
                    [Typ.var ~loc s; Typ.any ~loc; Typ.var ~loc "syn" ]
                  )
              in
              Typ.class_ ~loc
                (Lident (sprintf "class_%s" tdecl.ptype_name.txt))
                (List.concat params @
                 Typ.[var ~loc "inh"; var ~loc "syn"; any ~loc ])
          | _ -> assert false
        )
  in
  let subj_t = Typ.use_tdecl tdecl in
  Typ.(chain_arrow ~loc [ tr_t; var ~loc "inh"; subj_t; var ~loc "syn" ])

let make_gcata_sig ~loc ?(shortname=false) tdecl =
  let type_ = make_gcata_typ ~loc tdecl in
  let name = if shortname then "gcata"
    else Printf.sprintf "gcata_%s" tdecl.ptype_name.txt
  in
  Sig.value ~loc ~name type_


let make_gcata_str ~loc tdecl =
  let gcata_pat =
     Pat.var ~loc (sprintf "gcata_%s" tdecl.ptype_name.txt)
  in
  let ans k =
    Str.single_value ~loc
      gcata_pat
      (Exp.fun_list ~loc [Pat.var ~loc "tr"; Pat.var ~loc "inh"; Pat.var ~loc "subj"]
         k)
  in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _labels ->
        let methname = sprintf "do_%s" tdecl.ptype_name.txt in
        ans @@ Exp.app_list ~loc
          (Exp.send ~loc (Exp.ident ~loc "tr") (Located.mk ~loc methname))
          [Exp.ident ~loc "inh"; Exp.ident ~loc "t"]
      )
    ~onvariant:(fun cds ->
        ans @@ prepare_patt_match ~loc (Exp.ident ~loc "subj") (`Algebraic cds)
          (fun cd names ->
             List.fold_left ("inh"::names)
               ~init:(Exp.send ~loc (Exp.ident ~loc "tr")
                        (Located.mk ~loc @@ "c_" ^ cd.pcd_name.txt))
               ~f:(fun acc arg -> Exp.app ~loc acc (Exp.ident ~loc arg))
        )
      )
    ~onmanifest:(fun typ ->
      let rec do_typ t = match t.ptyp_desc with
      | Ptyp_alias (t,_) -> do_typ t
      | Ptyp_constr ({txt},_) ->
          Str.single_value ~loc
               gcata_pat
               (Exp.of_longident ~loc @@
                map_longident txt ~f:(fun s -> "gcata_"^s) )
      | Ptyp_tuple ts ->
        (* let's say we have predefined aliases for now *)
        do_typ @@ constr_of_tuple ~loc:t.ptyp_loc ts

      | Ptyp_variant (rows,_,maybe_labels) ->
          ans @@ prepare_patt_match_poly ~loc (Exp.ident ~loc "t") rows maybe_labels
            ~onrow:(fun cname names ->
              List.fold_left ("inh"::(List.map ~f:fst names))
                ~init:(Exp.send ~loc (Exp.ident ~loc "tr")
                         (Located.mk ~loc @@ "c_" ^ cname) )
                ~f:(fun acc arg ->
                       Exp.app ~loc acc (Exp.ident ~loc arg)
                   )
            )
            ~onlabel:(fun label patname -> failwith "not implemented")
            ~oninherit:(fun params cident patname ->
                Exp.app_list ~loc
                  (Exp.of_longident ~loc  @@
                   map_longident cident ~f:(sprintf "gcata_%s"))
                  (List.map ["tr";"inh";patname] ~f:(Exp.sprintf ~loc "%s"))

              )
      | _ -> failwith "not implemented"
      in
      do_typ typ
      )

(* create opened renaming for polymorphic variant *)
(* Seems that we don't need it no more *)
let make_heading_gen ~loc wrap tdecl = []

(* let make_heading_str ~loc = make_heading_gen ~loc (Str.type_ ~loc Nonrecursive)
 * let make_heading_sig ~loc = make_heading_gen ~loc (Sig.type_ ~loc Nonrecursive) *)

let name_fcm_mt tdecl = sprintf "MT_%s" tdecl.ptype_name.txt

(* Part of old implementation where we are trying to collect all values in t
 * first-class module. But we decided to roll back because we can't write
 * generic function to access it *)
(* let gather_module_str tdecl plugins =
 *   let loc = tdecl.ptype_loc in
 *
 *   let body = [%stri let gcata =
 *                       [%e Exp.sprintf "gcata_%s" tdecl.ptype_name.txt] ] ::[]
 *   in
 *   let body = List.fold_left ~init:body plugins
 *       ~f:(fun acc p ->
 *         let expr = Exp.sprintf ~loc "%s" @@ p#make_trans_function_name tdecl in
 *         Str.single_value ~loc (Pat.of_string ~loc p#plugin_name) expr
 *         :: acc
 *       )
 *   in
 *   let expr = Exp.pack_with_constraint ~loc
 *       (Mod.structure ~loc @@ List.rev body)
 *       (Located.lident ~loc (name_fcm_mt tdecl))
 *   in
 *   Str.single_value ~loc (Pat.sprintf "%s" tdecl.ptype_name.txt) expr *)

(* let make_fcm_sig ~loc tdecl plugins =
 *   let fields = List.concat_map plugins ~f:(fun p ->
 *       let name  = p#plugin_name in
 *       let type_ = p#make_trans_function_typ tdecl in
 *       [Sig.value ~loc ~name type_ ]
 *     )
 *   in
 *   Mty.signature ~loc ((make_gcata_sig ~shortname:true ~loc tdecl) :: fields )
 *
 * let prepare_mt ~loc tdecl plugins =
 *     let name = Located.mk ~loc @@ sprintf "MT_%s" tdecl.ptype_name.txt in
 *     let type_ = Some (make_fcm_sig ~loc tdecl plugins) in
 *     Str.modtype ~loc (module_type_declaration ~loc ~name ~type_) *)


let collect_plugins_str ~loc tdecl plugins : Str.t =
  let plugin_fields =
    List.map plugins ~f:(fun p ->
      Cf.method_concrete ~loc p#plugin_name @@
      Exp.sprintf ~loc "%s" @@ p#make_trans_function_name tdecl)
  in

  let tname = tdecl.ptype_name.txt in
  Str.single_value ~loc (Pat.sprintf ~loc "%s" tname) @@
  Exp.record ~loc
    [ Ldot (lident "GT", "gcata"), Exp.sprintf ~loc "gcata_%s" tname
    ; Ldot (lident "GT", "plugins"), Exp.object_ ~loc @@ class_structure
        ~self:(Pat.any ~loc ()) ~fields:plugin_fields
    ]

let collect_plugins_sig ~loc tdecl plugins =
  Sig.value ~loc ~name:tdecl.ptype_name.txt @@
  Typ.constr ~loc (Located.mk ~loc (Ldot (lident "GT", "t")) )
    [ make_gcata_typ ~loc tdecl
    ; Typ.object_ ~loc Closed @@ List.map plugins ~f:(fun p ->
        (p#plugin_name, p#make_trans_function_typ ~loc tdecl)
      )
    ]

(* for structures *)
let do_typ ~loc sis plugins is_rec tdecl =
  let intf_class = make_interface_class ~loc tdecl in
  let gcata = make_gcata_str ~loc tdecl in

  (* let decls = [] in *)
  (* let decls = match Str.tdecl ~loc ~name ~params (MidiAst.ctyp_of tinfo) with
   * | Some si -> [si]
   * | None -> []
   * in *)
  List.concat
    [ sis
    ; [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single ~loc ~is_rec tdecl)
    ; [ collect_plugins_str ~loc tdecl plugins ]
    ]

(* let (_:int) = collect_plugins_str *)

let do_mutal_types ~loc plugins tdecls =
  List.concat_map tdecls ~f:(fun tdecl ->
    (* make_heading_str ~loc tdecl @ *)
    [ make_interface_class ~loc tdecl
    ; make_gcata_str ~loc tdecl ]
  ) @
  List.concat_map plugins ~f:(fun g -> g#do_mutals ~loc ~is_rec:true tdecls)


(* for signatures *)
let do_typ_sig ~loc plugins is_rec tdecl =
  let intf_class = make_interface_class_sig ~loc tdecl in
  let gcata = make_gcata_sig ~loc tdecl in

  (* let mt_name = sprintf "MT_%s" tdecl.ptype_name.txt in
   * let prepare_mt tdecl plugins =
   *   let name = Located.mk ~loc mt_name in
   *   let type_ = Some (make_fcm_sig ~loc tdecl plugins) in
   *   Sig.modtype ~loc (module_type_declaration ~loc ~name ~type_)
   * in
   * let module_itself =
   *   Sig.value ~loc ~name:tdecl.ptype_name.txt @@
   *     Typ.package ~loc (Located.lident ~loc mt_name)
   * in *)
  List.concat
    [ (* make_heading_sig ~loc tdecl
     * ; *) [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single_sig ~loc ~is_rec tdecl)
    ; [ collect_plugins_sig ~loc tdecl plugins ]
    (* ; [prepare_mt tdecl plugins; module_itself] *)
    ]

let do_mutal_types_sig ~loc plugins tdecls =
  []
  (* List.concat_map tdecls ~f:(fun tdecl ->
   *   make_heading_str ~loc tdecl @
   *   [ make_interface_class ~loc tdecl
   *   (\* ; make_gcata_sig ~loc tdecl *\)
   *   ]
   * ) @
   * List.concat_map plugins ~f:(fun g -> g#do_mutals ~loc ~is_rec:true tdecls) *)

let registered_plugins : (string * _ ) list =
  [ (let module M = Show   .Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ; (let module M = Compare.Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ;  (let module M = Gmap   .Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ; (let module M = Foldl  .Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ; (let module M = Show_typed.Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ; (let module M = Eq     .Make(AstHelpers) in
     M.plugin_name, (M.g :> (Plugin_intf.plugin_args -> _ Plugin_intf.Make(AstHelpers).t)) )
  ]

let wrap_plugin name = function
| Skip -> id
| Use args ->
    match List.Assoc.find registered_plugins name ~equal:String.equal with
    | Some p -> List.cons (p args :> _ Plugin_intf.Make(AstHelpers).t)
    | None -> failwithf "Plugin '%s' is not registered" name ()
;;

let sig_type_decl ~loc ~path
    ?(use_show=Skip) ?(use_gmap=Skip) ?(use_foldl=Skip) ?(use_show_type=Skip)
    ?(use_compare=Skip) ?(use_eq=Skip)
    (rec_flag, tdls) =
  let plugins =
    wrap_plugin "show"        use_show  @@
    wrap_plugin "compare"     use_compare @@
    wrap_plugin "gmap"        use_gmap  @@
    wrap_plugin "foldl"       use_foldl @@
    wrap_plugin "show_typed"  use_show_type @@
    wrap_plugin "eq"          use_eq @@
    []
  in
  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ_sig ~loc plugins true tdecl
  | Recursive, ts      -> do_mutal_types_sig ~loc plugins ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ_sig ~loc plugins false) tdls


let str_type_decl ~loc ~path si
    ?(use_show=Skip) ?(use_gmap=Skip) ?(use_foldl=Skip) ?(use_show_type=Skip)
    ?(use_compare=Skip) ?(use_eq=Skip)
    (rec_flag, tdls)
  =
  let plugins =
    wrap_plugin "show"        use_show  @@
    wrap_plugin "compare"     use_compare @@
    wrap_plugin "gmap"        use_gmap  @@
    wrap_plugin "foldl"       use_foldl @@
    wrap_plugin "show_typed"  use_show_type @@
    wrap_plugin "eq"          use_eq @@
    []
  in

  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ ~loc si plugins true tdecl
  | Recursive, ts      -> do_mutal_types ~loc plugins ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ ~loc si plugins false) tdls

let str_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl
    use_compare use_eq use_show_type =
  str_type_decl ~loc ~path info ~use_show ~use_gmap ~use_foldl ~use_show_type
    ~use_compare ~use_eq

let sig_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl
    use_compare use_eq use_show_type =
  let wrap f = if f then Use [] else Skip in
  sig_type_decl ~loc ~path
    ~use_show: (wrap use_show)
    ~use_gmap: (wrap use_gmap)
    ~use_foldl:(wrap use_foldl)
    ~use_compare:(wrap use_compare)
    ~use_show_type:(wrap use_show_type)
    ~use_eq:(wrap use_eq)
    info

end
