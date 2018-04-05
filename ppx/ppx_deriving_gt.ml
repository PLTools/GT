(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib

module Type_conv = Ppxlib.Deriving

module E = Ppx_gt_expander
let name = "gt"

  (* (\* This doesn't works because there is no place for pexp_apply *\)
   * let gt_param name =
   *   let open Type_conv.Args in
   *   let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
   *   arg name @@
   *   pexp_apply
   *     (pexp_ident (lident (string name)))
   *     ( (pair nolabel r) ^:: nil )
   *
   * let gt_param name =
   *   let open Type_conv.Args in
   *   let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
   *   arg name @@
   *   pexp_apply
   *     (pexp_ident (lident (string name)))
   *     ( (pair nolabel r) ^:: nil ) *)

let gt_paramA name =
  let open Type_conv.Args in
  let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
  (* let b = alt none (some r) in *)
  (* make_param name (assert false) (Ast_pattern.Packed.create b (fun x -> x)) *)
  arg (name^"A") r
  (* alt none (some r) *)

  (* without arguments *)
let gt_param name =
  let open Type_conv.Args in
  flag name


  (* let make_param name =
   *   let open Type_conv.Args in
   *
   *   (\* let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in *\)
   *   let pattern = pexp_ident (lident (string name)) in
   *   Type_conv.Args.make_param
   *     name
   *     (let loc = Location.none in [%expr { wtf=1 } ])
   *     @@
   *     Ast_pattern.Packed.create pattern (fun x -> x) *)



  (* let make_param name : (int,_) Type_conv.Generator.t  =
   *   let open Type_conv.Args in
   *
   *   let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
   *
   *   Type_conv.Generator.make
   *     (arg name
   *        (\* (many *\)
   *           (pexp_apply
   *              (pexp_ident (lident (string "name")) )
   *              ( (pair nolabel r) ^:: nil )
   *           )
   *        (\* ) *\)
   *     )
   *     (\* (fun ~loc ~path info x  -> x) *\) *)


  let str_type_decl : (_, _) Type_conv.Generator.t =
    let bothp name rest =
      Type_conv.Args.(rest +> (gt_param name) +> (gt_paramA name))
    in
    Type_conv.Generator.make
      (* ~attributes:[ Attribute.T Attrs.ignore ] *)
      Type_conv.Args.(empty
                      (* +> (arg "show"  (pexp_record __ none))
                       * +> (arg "gmap"  (pexp_record __ none))
                       * +> (arg "foldl" (pexp_record __ none)) *)
                      |> (bothp "show")
                      |> (bothp "gmap")
                      |> (bothp "foldl")
                      |> (bothp "show_typed")
                     )
      (fun ~loc ~path info show showA gmap gmapA foldl foldlA show_type show_typeA ->
         let wrap = function
         | _,Some xs -> E.Use xs
         | true,None -> E.Use []
         | false,None -> E.Skip
         in
         let show  = wrap (show,showA) in
         let gmap  = wrap (gmap,gmapA) in
         let foldl = wrap (foldl,foldlA) in
         let show_type = wrap (show_type,show_typeA) in
         E.str_type_decl_implicit ~loc ~path info show gmap foldl show_type
      )

  let sig_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      Type_conv.Args.(empty +> flag "show" +> flag "gmap" +> flag "foldl" +> flag "show_typed"
                     )
      E.sig_type_decl_implicit

  let deriver =
    Type_conv.add name
      ~str_type_decl
      ~sig_type_decl

