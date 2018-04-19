(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib

module E = Ppx_gt_expander

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
  let open Deriving.Args in
  let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
  arg (name^"A") r

(* without arguments *)
let gt_param name =
  let open Deriving.Args in
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


let str_type_decl : (_, _) Deriving.Generator.t =
  let bothp name rest =
    Deriving.Args.(rest +> (gt_param name) +> (gt_paramA name))
  in
  Deriving.Generator.make
    (* ~attributes:[ Attribute.T Attrs.ignore ] *)
    Deriving.Args.(empty
                   |> (bothp "show")
                   |> (bothp "gmap")
                   |> (bothp "foldl")
                   |> (bothp "show_typed")
                   |> (bothp "compare")
                  )
    (fun ~loc ~path info show showA gmap gmapA foldl foldlA show_type show_typeA
      compare compareA ->
      let wrap = function
      | _,Some xs -> E.Use xs
      | true,None -> E.Use []
      | false,None -> E.Skip
      in
      let show  = wrap (show,showA) in
      let gmap  = wrap (gmap,gmapA) in
      let foldl = wrap (foldl,foldlA) in
      let show_type = wrap (show_type,show_typeA) in
      let compare   = wrap (compare,compareA) in
      E.str_type_decl_implicit ~loc ~path
        info show gmap foldl compare show_type
    )

let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make
    Deriving.Args.(empty
                   +> flag "show" +> flag "gmap" +> flag "foldl"
                   +> flag "compare" +> flag "show_typed"
                  )
    E.sig_type_decl_implicit

let () =
  (* Sys.command "notify-send 'Registering deriver' wtf" |> ignore; *)
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    "gt"
  |> Deriving.ignore
