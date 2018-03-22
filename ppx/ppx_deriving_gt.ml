(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core

module Type_conv = Ppx_type_conv.Std.Type_conv

(* module Attrs = struct
 *   let ignore =
 *     Attribute.declare "hash.ignore"
 *       Attribute.Context.type_declaration
 *       Ast_pattern.(pstr ((pstr_value nonrecursive
 *                            (value_binding ~pat:(pstring __) ~expr:__ ^:: nil) ) ^:: nil)
 *                   )
 *       (fun (s: string) (e:expression) -> assert false; 1)
 *
 * end *)

module Sexp_of = struct
  module E = Ppx_gt_expander
  let name = "gt"


  let make_param name =
    let open Type_conv.Args in

    let r = pexp_record (many (map1 ~f:(fun (l,e) -> (l.txt,e) ) __)) none in
    arg name @@
    pexp_apply
      (pexp_ident (lident (string name)))
      ( (pair nolabel r) ^:: nil )


  let str_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      (* ~attributes:[ Attribute.T Attrs.ignore ] *)
      Type_conv.Args.(empty (* +> flag "show" +> flag "gmap" +> flag "foldl" *)
                      +> (make_param "show") +> (make_param "gmap")
                     )
      (fun ~loc ~path info show gmap ->
         let wrap = function Some xs -> xs | None -> [] in
         let show = wrap show in
         let gmap = wrap gmap in

         E.str_type_decl_implicit ~loc ~path info show gmap
      )

  let sig_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      Type_conv.Args.(empty +> flag "show" +> flag "gmap"
                     )
      E.sig_type_decl_implicit

  let deriver =
    Type_conv.add name
      ~str_type_decl
      ~sig_type_decl

end
