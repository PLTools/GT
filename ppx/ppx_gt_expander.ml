(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core
open Ppx_core.Ast_builder.Default
open Printf
open Longident
open Asttypes
open Ast_helper
open GtHelpers
let (@@) = Caml.(@@)

let deriver = "gt"

let make_interface_class_sig ~loc tdecl =
  let loc = tdecl.ptype_loc in
  let params = List.map ~f:fst tdecl.ptype_params in
  let name = tdecl.ptype_name in

  let k ?(is_poly=false) fields =
    let prepare_params is_poly =
      let middle = if is_poly then [[%type: 'polyvar_extra]] else [] in
      prepare_param_triples ~loc ~middle params
    in
    Sig.class_ ~loc ~virt:Virtual
      ~name:(sprintf "class_%s" name.txt)
      ~params:(prepare_params is_poly |> invariantize)
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
                ; if is_poly then [[%type: 'polyvar_extra]] else []
                ]
            in

            [ Ctf.inherit_ ~loc @@
              Cty.constr (mknoloc @@ map_longident ~f:(sprintf "class_%s") name)
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
                     wrap ~is_poly:true txt params
                  | _ -> assert false
                )
              in
              k ~is_poly:true meths
          | _ -> failwith "not implemented"
          in
          helper typ
    )


let make_interface_class ~loc root_type =
  let params = List.map ~f:fst root_type.ptype_params in
  let name = root_type.ptype_name in

  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (if polyvar)
  *)
  let prepare_params is_poly =
    let middle = if is_poly then [[%type: 'polyvar_extra]] else [] in
    prepare_param_triples ~loc ~middle params
  in

  let ans ?(is_poly=false) fields =
    Str.class_single ~loc ~name:(sprintf "class_%s" name.txt) fields
      ~params:(prepare_params is_poly |> invariantize)
  in
  visit_typedecl ~loc root_type
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
                ; if is_poly then [[%type: 'polyvar_extra]] else []
                ]
            in

            [ Cf.inherit_ ~loc @@
              Cl.constr (mknoloc @@ map_longident ~f:(sprintf "class_%s") name)
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
                | Rtag (lab,_,_,args)  ->
                  let methname = sprintf "c_%s" lab in
                  [ Cf.method_ ~loc methname @@ Cfk_virtual
                      (List.fold_right args ~init:[%type: 'syn]
                         ~f:(Typ.arrow Nolabel)
                       |> (Typ.arrow Nolabel [%type: 'inh])
                      )
                  ]
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


let make_gcata_sig ~loc tdecl =
  let tr_t = [%type: < .. > ] in
  let tr_t = visit_typedecl ~loc tdecl
      ~onvariant:(fun cds ->
          Typ.object_ ~loc Open @@
          List.map cds
            ~f:(fun cd ->
                match cd.pcd_args with
                | Pcstr_tuple ts -> cd.pcd_name.txt, Typ.chain_arrow ~loc (ts@[[%type: int]])
                | Pcstr_record _ -> assert false
              )
        )
      ~onmanifest:(fun typ ->
          match typ.ptyp_desc with
          | Ptyp_constr ({txt;loc}, ts) ->
            (* there we can fuck up extra argument about polymorphic variants *)
            let args = List.concat_map ts ~f:()
            Typ.class_ ~loc txt
        )
  in
  let subj_t = using_type ~typename:tdecl.ptype_name.txt tdecl in


  let type_ = Typ.chain_arrow ~loc [ tr_t; subj_t; [%type: 'inh]; [%type: 'syn] ] in
  Sig.value ~loc ~prim:[] ~name:"gcata" ~type_:type_


let make_gcata_str ~loc root_type =
  let gcata_pat =
     Pat.var ~loc (sprintf "gcata_%s" root_type.ptype_name.txt)
  in
  let ans k =
    Str.value ~loc Nonrecursive
      [value_binding ~loc
         ~pat:gcata_pat
         ~expr:[%expr fun tr inh t -> [%e k] ]
      ]
  in
  visit_typedecl ~loc root_type
    ~onvariant:(fun cds ->
      ans @@ prepare_patt_match ~loc [%expr t] (`Algebraic cds) (fun cd names ->
          List.fold_left ("inh"::names)
            ~init:(Exp.send ~loc [%expr tr] ("c_" ^ cd.pcd_name.txt))
            ~f:(fun acc arg -> Exp.apply ~loc acc [Nolabel, Exp.ident arg])
        )
      )
    ~onmanifest:(fun typ ->
      let rec do_typ t = match t.ptyp_desc with
      | Ptyp_alias (t,_) -> do_typ t
      | Ptyp_constr ({txt},_) ->
          Str.value ~loc Nonrecursive
            [value_binding ~loc
               ~pat:gcata_pat
               ~expr:(Exp.ident_of_long ~loc @@ mknoloc @@
                      map_longident txt ~f:(fun s -> "gcata_"^s) )
            ]
      | Ptyp_tuple ts ->
        (* let's say we have predefined aliases for now *)
        do_typ @@ constr_of_tuple ~loc ts

      | Ptyp_variant (rows,_,maybe_labels) ->
          ans @@ prepare_patt_match_poly ~loc [%expr t] rows maybe_labels
            ~onrow:(fun cname  names ->
              List.fold_left ("inh"::(List.map ~f:fst names))
                ~init:(Exp.send ~loc [%expr tr] ("c_" ^ cname))
                ~f:(fun acc arg ->
                       Exp.apply ~loc acc [Nolabel, Exp.ident arg]
                   )
            )
            ~onlabel:(fun label patname -> failwith "not implemented")
            ~oninherit:(fun params cident patname ->
                Exp.apply_nolabeled
                  (Exp.ident_of_long ~loc  @@
                   Located.mk ~loc@@map_longident cident ~f:(sprintf "gcata_%s"))
                  (List.map ["tr";"inh";patname] ~f:(Exp.sprintf ~loc "%s"))

              )
      | _ -> failwith "not implemented"
      in
      do_typ typ
    )

(* create opened renaming for polymorphc variant *)
let make_heading_gen ~loc wrap tdecl =
  visit_typedecl ~loc tdecl
    ~onvariant:(fun _ -> [])
    ~onmanifest:(fun typ -> match typ.ptyp_desc with
    | Ptyp_variant (fields,_,labels) ->
        [ wrap (* Str.type_ ~loc Nonrecursive *)
            [ let opened_t = Typ.variant ~loc fields Open labels in
              let self_t = [%type: 'self] in
              type_declaration ~loc
                ~name:(Located.map (sprintf "%s_open") tdecl.ptype_name)
                ~params:((self_t,Invariant) :: tdecl.ptype_params)
                ~manifest:(Some self_t)
                ~private_:Public
                ~kind:tdecl.ptype_kind
                ~cstrs:[(self_t,opened_t,loc)]
            ]
        ]
    | _ -> []
    )

let make_heading_str ~loc = make_heading_gen ~loc (Str.type_ ~loc Nonrecursive)
let make_heading_sig ~loc = make_heading_gen ~loc (Sig.type_ ~loc Nonrecursive)


(* for structures *)
let do_typ ~loc plugins is_rec tdecl =
  let intf_class = make_interface_class ~loc tdecl in
  let gcata = make_gcata_str ~loc tdecl in
  List.concat
    [ make_heading_str ~loc tdecl
    ; [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single ~loc ~is_rec tdecl)
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
  List.concat
    [ make_heading_sig ~loc tdecl
    ; [intf_class; gcata]
    ; List.concat_map plugins ~f:(fun g -> g#do_single_sig ~loc ~is_rec tdecl)
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

let sig_type_decl ~loc ~path (rec_flag, tdls)
    ?(use_show=false) ?(use_gmap=false) ?(use_foldl=false) =
  let plugins =
    let wrap f p = if f then List.cons p else id in
    wrap use_show  Show.g @@
    wrap use_gmap  Gmap.g @@
    []
  in
  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ_sig ~loc plugins true tdecl
  | Recursive, ts      -> do_mutal_types_sig ~loc plugins ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ_sig ~loc plugins false) tdls


let str_type_decl ~loc ~path (rec_flag, tdls)
    ?(use_show=false) ?(use_gmap=false) ?(use_foldl=false) =
  let plugins =
    let wrap f p = if f then List.cons p else id in
    wrap use_show  Show.g @@
    wrap use_gmap  Gmap.g @@
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

let str_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl =
  str_type_decl ~loc ~path info ~use_show ~use_gmap ~use_foldl

let sig_type_decl_implicit ~loc ~path info use_show use_gmap use_foldl =
  sig_type_decl ~loc ~path info ~use_show ~use_gmap ~use_foldl
