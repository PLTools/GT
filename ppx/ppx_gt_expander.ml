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

type supported_derivers = { gt_show: bool; gt_gmap: bool }

let make_interface_class ~loc root_type =
  let params = List.map ~f:fst root_type.ptype_params in
  (* let sort = root_type *)
  let name = root_type.ptype_name in
  (* let manifest = root_type.ptype_manifest in *)

  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (if polyvar)
  *)
  let prepare_params is_poly =
    if is_poly
    then failwith "polyvariants not yet work"
    else prepare_param_triples ~loc params
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
          let wrap name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("s"^n) )
                  ]
                )
              in
              let inh_params = inh_params @ inh_syn_ts ~loc () in
              [ Cf.inherit_ ~loc @@
                Cl.constr (mknoloc @@ map_longident ~f:(sprintf "class_%s") name)
                  inh_params
              ]
          in

          let rec helper typ = match typ with
          | {ptyp_desc=Ptyp_var name} -> not_implemented "antiphantom types"
          | [%type: string]
          | [%type: char]
          | [%type: int]  ->
              not_implemented "%s " Caml.__FILE__ (* Caml.__LINE__ *)
          | {ptyp_desc=Ptyp_constr ({txt;loc}, params)} ->
              (* a type alias on toplevel *)
              ans @@ wrap txt params
          | {ptyp_desc=Ptyp_alias (typ, new_name)} ->
              map_core_type typ ~onvar:(fun as_ ->
                if String.equal as_ new_name
                then Typ.constr (Located.lident ~loc name.txt) params
                else Typ.var ~loc as_
              ) |> helper
          | {ptyp_desc=Ptyp_variant (rows,_,labels)} ->
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
                     wrap txt params
                  | _ -> assert false
                )
              in
              ans meths
          | _ -> failwith "not implemented"
          in
          helper typ
    )


let make_gcata ~loc root_type =
  let gcata_pat =
     Pat.var (mknoloc @@
             sprintf "gcata_%s" root_type.ptype_name.txt)
  in
  visit_typedecl ~loc root_type
    ~onvariant:(fun cds ->
      let ans k =
        Str.value ~loc Nonrecursive
          [value_binding ~loc
             ~pat:gcata_pat
             ~expr:[%expr fun tr inh t -> [%e k] ]
          ]
      in
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
      | Ptyp_variant (rows,_,maybe_labels) ->
          let ans k =
            Str.value ~loc Nonrecursive
              [value_binding ~loc
                 ~pat:gcata_pat
                 ~expr:[%expr fun tr inh t -> [%e k] ]
              ]
          in
          ans @@ prepare_patt_match_poly ~loc [%expr t] rows maybe_labels
            ~onrow:(fun cname  names ->
              List.fold_left ("inh"::(List.map ~f:fst names))
                ~init:(Exp.send ~loc [%expr tr] ("c_" ^ cname))
                ~f:(fun acc arg ->
                       Exp.apply ~loc acc [Nolabel, Exp.ident arg]
                   )
            )
            ~onlabel:(fun label patname ->
                Exp.apply_nolabeled ~loc (Exp.sprintf "gcata_%s" label) []
              )
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

let do_typ ~loc options is_rec root_type =
  let intf_class = make_interface_class ~loc root_type in
  let gcata = make_gcata ~loc root_type in
  intf_class :: gcata ::
  (if options.gt_show
  then Show.do_single ~loc ~is_rec:true root_type
  else [])


let do_mutal_types ~loc options tdecls =
  List.concat_map tdecls ~f:(fun tdecl ->
    let intf_class = make_interface_class ~loc tdecl in
    let gcata = make_gcata ~loc tdecl in
    [intf_class; gcata]
  ) @

  (if options.gt_show
  then Show.do_mutals ~loc ~is_rec:true tdecls
  else [])

let str_type_decl ~loc ~path (rec_flag, tdls) gt_show gt_gmap =
  let options = {gt_show; gt_gmap} in
  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ ~loc options true tdecl
  | Recursive, ts      -> do_mutal_types ~loc options ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ ~loc options false) tdls
