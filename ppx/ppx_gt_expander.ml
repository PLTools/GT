(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Base
open Ppxlib
open Ppxlib.Ast_builder.Default
open Printf
open Longident
open Asttypes
open Ast_helper
open GtHelpers
let (@@) = Caml.(@@)

let deriver = "gt"

type config_plugin = Skip | Use of (longident * expression) list

let make_interface_class_sig ~loc tdecl =
  let loc = tdecl.ptype_loc in
  let params = List.map ~f:fst tdecl.ptype_params in
  let name = tdecl.ptype_name in

  let k fields =
    let prepare_params =
      let middle = [[%type: 'extra]] in
      prepare_param_triples ~loc ~middle tdecl.ptype_params
    in
    Sig.class_ ~loc ~virt:Virtual
      ~name:(sprintf "class_%s" name.txt)
      ~params:(prepare_params |> invariantize)
      fields

  in
  visit_typedecl ~loc tdecl
    ~onvariant:(fun cds ->
      k @@
      List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "record constructors"
        | Pcstr_tuple ts ->
            Ctf.method_ ~loc methname
               (List.fold_right ts ~init:[%type: 'syn] ~f:(Typ.arrow Nolabel)
                |> (Typ.arrow Nolabel [%type: 'inh])
               )
      )
    )
    ~onmanifest:(fun typ ->
          let wrap name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("s"^n) )
                  ]
                )
            in
            let inh_params = List.concat
                [ inh_params
                ; inh_syn_ts ~loc ()
                ; [[%type: 'extra]]
                ]
            in

            [ Ctf.inherit_ ~loc @@
              Cty.constr (Located.mk ~loc @@
                          map_longident ~f:(sprintf "class_%s") name)
                inh_params
            ]
          in

          let rec helper typ = match typ with
          | [%type: string]
          | [%type: char]
          | [%type: int]  ->
            not_implemented "%s " Caml.__FILE__ (* Caml.__LINE__ *)
          | _ -> match typ.ptyp_desc with
          | Ptyp_var name -> not_implemented "antiphantom types"
          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            k @@ wrap txt params
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let loc = typ.ptyp_loc in
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts) in
            helper @@ Typ.constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
              map_core_type typ ~onvar:(fun as_ ->
                if String.equal as_ new_name
                then Typ.constr (Located.lident ~loc name.txt) params
                else Typ.var ~loc as_
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
              let meths =
                List.concat_map rows ~f:(function
                | Rtag (lab,_,_,args)  ->
                  let methname = sprintf "c_%s" lab in
                  [ Ctf.method_ ~loc methname @@
                      (List.fold_right args ~init:[%type: 'syn]
                         ~f:(Typ.arrow Nolabel)
                       |> (Typ.arrow Nolabel [%type: 'inh])
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

  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (if polyvar)
  *)
  let prepare_params =
    let middle = [[%type: 'extra]] in
    prepare_param_triples ~loc ~middle tdecl.ptype_params
  in

  let ans ?(is_poly=false) fields =
    Str.class_single ~loc ~name:(sprintf "class_%s" name.txt) fields
      ~params:(prepare_params |> invariantize)
  in
  visit_typedecl ~loc tdecl
    ~onvariant:(fun cds ->
      ans @@
      List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "record constructors"
        | Pcstr_tuple ts ->
            Cf.method_ ~loc methname ~flg:Public @@ Cfk_virtual
               (List.fold_right ts ~init:[%type: 'syn] ~f:(Typ.arrow Nolabel)
                |> (Typ.arrow Nolabel [%type: 'inh])
               )
      )
    )
    ~onmanifest:(fun typ ->
          let wrap ?(is_poly=false) name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("s"^n) )
                  ]
                )
            in
            let inh_params = List.concat
                [ inh_params
                ; inh_syn_ts ~loc ()
                ; [Plugin.construct_extra_param ~loc]
                ]
            in

            [ Cf.inherit_ ~loc @@
              Cl.constr ~loc (map_longident ~f:(sprintf "class_%s") name)
                inh_params
            ]
          in

          let rec helper typ = match typ with
          | [%type: string]
          | [%type: char]
          | [%type: int]  ->
            not_implemented "%s " Caml.__FILE__
          | _ -> match typ.ptyp_desc with
          | Ptyp_var name -> not_implemented "antiphantom types"
          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            ans @@ wrap txt params
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let loc = typ.ptyp_loc in
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts) in
            helper @@ Typ.constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
              map_core_type typ ~onvar:(fun as_ ->
                if String.equal as_ new_name
                then Typ.constr (Located.lident ~loc name.txt) params
                else Typ.var ~loc as_
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
              let meths =
                List.concat_map rows ~f:(function
                    (* | Rtag (lab,_,_,args) when Int.(>) (List.length args) 1 -> *)
                    | Rtag (lab,_,_,[typ]) ->
                      (* print_endline "HERE"; *)
                      let args = match typ.ptyp_desc with
                        | Ptyp_tuple ts -> ts
                        | _ -> [typ]
                      in
                      let methname = sprintf "c_%s" lab in
                      [ Cf.method_ ~loc methname @@ Cfk_virtual
                          (List.fold_right args ~init:[%type: 'syn]
                             ~f:(Typ.arrow Nolabel)
                           |> (Typ.arrow Nolabel [%type: 'inh])
                          )
                      ]
                    | Rtag (_,_,_,_args) ->
                      failwith "Can't deal with conjunctive types"

                    | Rinherit typ -> match typ.ptyp_desc with
                      | Ptyp_constr ({txt;loc}, params) ->
                        wrap ~is_poly:true txt params
                      | _ -> assert false
                  )
              in
              ans ~is_poly:true meths
          | _ -> failwith "not implemented"
          in
          helper typ
    )


let make_gcata_sig ~loc ?(shortname=false) tdecl =
  let tr_t = visit_typedecl ~loc tdecl
      ~onvariant:(fun cds ->
          Typ.object_ ~loc Open @@
          List.map cds
            ~f:(fun cd ->
              match cd.pcd_args with
              | Pcstr_tuple ts ->
                  Located.mk ~loc @@ sprintf "c_%s" cd.pcd_name.txt,
                  Typ.chain_arrow ~loc ([%type: 'inh]::ts@[[%type: 'syn]])
              | Pcstr_record _ -> assert false
            )
        )
      ~onmanifest:(fun typ ->
          match typ.ptyp_desc with
          | Ptyp_constr ({txt;loc}, ts) ->
            (* there we can fuck up extra argument about polymorphic variants *)
            let args = map_type_param_names tdecl.ptype_params ~f:(fun name ->
                [ Typ.var ~loc name
                ; Typ.any ~loc ()
                ; Typ.var ~loc @@ "s"^name ]
              ) |> List.concat
            in
            let args = args @ [ [%type: 'inh]; [%type: 'syn]; [%type: _]] in
            Typ.class_ ~loc (Lident(sprintf "class_%s" tdecl.ptype_name.txt)) args
          | Ptyp_variant (rows,_flg,_) ->
              let params = map_type_param_names tdecl.ptype_params
                  ~f:(fun s ->
                    [Typ.var ~loc s; Typ.any ~loc (); [%type: 'syn]]
                  )
              in
              Typ.class_ ~loc
                (Lident (sprintf "class_%s" tdecl.ptype_name.txt))
                (List.concat params @
                 Typ.[var ~loc "inh"; var ~loc "syn"; any ~loc ()])
          | _ -> assert false
        )
  in
  let subj_t = using_type ~typename:tdecl.ptype_name.txt tdecl in

  let type_ = Typ.chain_arrow ~loc [ tr_t; [%type: 'inh]; subj_t; [%type: 'syn] ] in
  let name = if shortname then "gcata" else Printf.sprintf "gcata_%s" tdecl.ptype_name.txt
  in
  Sig.value ~loc ~name type_


let make_gcata_str ~loc root_type =
  let gcata_pat =
     Pat.var ~loc (sprintf "gcata_%s" root_type.ptype_name.txt)
  in
  let ans k =
    Str.single_value ~loc
         gcata_pat
         [%expr fun tr inh t -> [%e k] ]
  in
  visit_typedecl ~loc root_type
    ~onvariant:(fun cds ->
      ans @@ prepare_patt_match ~loc [%expr t] (`Algebraic cds) (fun cd names ->
          List.fold_left ("inh"::names)
            ~init:(Exp.send ~loc [%expr tr] (Located.mk ~loc @@ "c_" ^ cd.pcd_name.txt))
            ~f:(fun acc arg -> Exp.apply ~loc acc [Nolabel, Exp.ident arg])
        )
      )
    ~onmanifest:(fun typ ->
      let rec do_typ t = match t.ptyp_desc with
      | Ptyp_alias (t,_) -> do_typ t
      | Ptyp_constr ({txt},_) ->
          Str.single_value ~loc
               gcata_pat
               (Exp.ident_of_long ~loc @@
                map_longident txt ~f:(fun s -> "gcata_"^s) )
      | Ptyp_tuple ts ->
        (* let's say we have predefined aliases for now *)
        do_typ @@ constr_of_tuple ~loc ts

      | Ptyp_variant (rows,_,maybe_labels) ->
          ans @@ prepare_patt_match_poly ~loc [%expr t] rows maybe_labels
            ~onrow:(fun cname names ->
              List.fold_left ("inh"::(List.map ~f:fst names))
                ~init:(Exp.send ~loc [%expr tr] (Located.mk ~loc @@ "c_" ^ cname))
                ~f:(fun acc arg ->
                       Exp.apply ~loc acc [Nolabel, Exp.ident arg]
                   )
            )
            ~onlabel:(fun label patname -> failwith "not implemented")
            ~oninherit:(fun params cident patname ->
                Exp.apply_nolabeled
                  (Exp.ident_of_long ~loc  @@
                   map_longident cident ~f:(sprintf "gcata_%s"))
                  (List.map ["tr";"inh";patname] ~f:(Exp.sprintf ~loc "%s"))

              )
      | _ -> failwith "not implemented"
      in
      do_typ typ
    )

(* create opened renaming for polymorphic variant *)
(* Seems that we don't need it no more *)
let make_heading_gen ~loc wrap tdecl =
  visit_typedecl ~loc tdecl
    ~onvariant:(fun _ -> [])
    ~onmanifest:(fun _ -> [])

let make_heading_str ~loc = make_heading_gen ~loc (Str.type_ ~loc Nonrecursive)
let make_heading_sig ~loc = make_heading_gen ~loc (Sig.type_ ~loc Nonrecursive)

let name_fcm_mt tdecl = sprintf "MT_%s" tdecl.ptype_name.txt

let gather_module_str tdecl plugins =
  let loc = tdecl.ptype_loc in

  let body = [%stri let gcata =
                      [%e Exp.sprintf "gcata_%s" tdecl.ptype_name.txt] ] ::[]
  in
  let body = List.fold_left ~init:body plugins
      ~f:(fun acc p ->
        let expr = Exp.sprintf ~loc "%s" @@ p#make_trans_function_name tdecl in
        Str.single_value ~loc (Pat.of_string ~loc p#plugin_name) expr
        :: acc
      )
  in
  let expr = Exp.pack_with_constraint ~loc
      (Mod.structure ~loc @@ List.rev body)
      (Located.lident ~loc (name_fcm_mt tdecl))
  in
  Str.single_value ~loc (Pat.sprintf "%s" tdecl.ptype_name.txt) expr

let make_fcm_sig ~loc tdecl plugins =
  let fields = List.concat_map plugins ~f:(fun p ->
      let name  = p#plugin_name in
      let type_ = p#make_trans_function_typ              tdecl in
      [Sig.value ~loc ~name type_ ]
    )
  in
  Mty.signature ~loc ((make_gcata_sig ~shortname:true ~loc tdecl) :: fields )

(* for structures *)
let do_typ ~loc plugins is_rec tdecl =
  let intf_class = make_interface_class ~loc tdecl in
  let gcata = make_gcata_str ~loc tdecl in

  let prepare_mt tdecl plugins =
    let name = Located.mk ~loc @@ sprintf "MT_%s" tdecl.ptype_name.txt in
    let type_ = Some (make_fcm_sig ~loc tdecl plugins) in
    Str.modtype ~loc (module_type_declaration ~loc ~name ~type_)
  in

  List.concat
    [ make_heading_str ~loc tdecl
    ; [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single ~loc ~is_rec tdecl)
    ; [prepare_mt tdecl plugins; gather_module_str tdecl plugins]
    ]

let do_mutal_types ~loc plugins tdecls =
  List.concat_map tdecls ~f:(fun tdecl ->
    make_heading_str ~loc tdecl @
    [ make_interface_class ~loc tdecl
    ; make_gcata_str ~loc tdecl ]
  ) @
  List.concat_map plugins ~f:(fun g -> g#do_mutals ~loc ~is_rec:true tdecls)


(* for signatures *)
let do_typ_sig ~loc plugins is_rec tdecl =
  let intf_class = make_interface_class_sig ~loc tdecl in
  let gcata = make_gcata_sig ~loc tdecl in

  let mt_name = sprintf "MT_%s" tdecl.ptype_name.txt in
  let prepare_mt tdecl plugins =
    let name = Located.mk ~loc mt_name in
    let type_ = Some (make_fcm_sig ~loc tdecl plugins) in
    Sig.modtype ~loc (module_type_declaration ~loc ~name ~type_)
  in
  let module_itself =
    Sig.value ~loc ~name:tdecl.ptype_name.txt @@
      Typ.package ~loc (Located.lident ~loc mt_name)
  in
  List.concat
    [ make_heading_sig ~loc tdecl
    ; [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single_sig ~loc ~is_rec tdecl)
    ; [prepare_mt tdecl plugins; module_itself]
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

let sig_type_decl ~loc ~path
    ?(use_show=Skip) ?(use_gmap=Skip) ?(use_foldl=Skip) ?(use_show_type=Skip)
    (rec_flag, tdls) =
  let plugins =
    let wrap p = function
    | Skip -> id
    | Use args -> List.cons (p args)
    in
    wrap Show.g       use_show  @@
    wrap Gmap.g       use_gmap  @@
    wrap Foldl.g      use_foldl @@
    wrap Show_typed.g use_show_type @@
    (* wrap use_foldl Eq.g @@ *)
    []
  in
  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ_sig ~loc plugins true tdecl
  | Recursive, ts      -> do_mutal_types_sig ~loc plugins ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ_sig ~loc plugins false) tdls


let str_type_decl ~loc ~path
    ?(use_show=Skip) ?(use_gmap=Skip) ?(use_foldl=Skip) ?(use_show_type=Skip)
    (rec_flag, tdls)
  =
  let plugins =
    let wrap p = function
    | Skip -> id
    | Use args -> List.cons (p args)
    in
    wrap Show.g       use_show @@
    wrap Gmap.g       use_gmap @@
    wrap Foldl.g      use_foldl @@
    wrap Show_typed.g use_show_type @@
    (* wrap use_foldl Foldl.g @@ *)
    (* wrap use_foldl Eq.g @@ *)
    []
  in

  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ ~loc plugins true tdecl
  | Recursive, ts      -> do_mutal_types ~loc plugins ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ ~loc plugins false) tdls

let str_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl use_show_type =
  str_type_decl ~loc ~path info ~use_show ~use_gmap ~use_foldl ~use_show_type

let sig_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl use_show_type =
  let wrap f = if f then Use [] else Skip in
  sig_type_decl ~loc ~path
    ~use_show: (wrap use_show)
    ~use_gmap: (wrap use_gmap)
    ~use_foldl:(wrap use_foldl)
    ~use_show_type:(wrap use_show_type)
    info
