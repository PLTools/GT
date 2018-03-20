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

module Attrs = struct
  let ignore =
    Attribute.declare "hash.ignore"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_value nonrecursive
                     (value_binding ~pat:(pstring __) ~expr:__ ^:: nil) )
      ()

  (* let no_hashing =
   *   Attribute.declare "hash.no_hashing"
   *     Attribute.Context.label_declaration
   *     Ast_pattern.(pstr nil)
   *     ()
   * ;; *)
end

module Sexp_of = struct
  module E = Ppx_gt_expander
  let name = "gt"

  let str_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      ~attributes:[ Attribute.T Attrs.ignore ]
      Type_conv.Args.(empty +> flag "show" +> flag "gmap" +> flag "foldl")
      E.str_type_decl_implicit

  let sig_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      Type_conv.Args.(empty +> flag "show" +> flag "gmap" +> flag "foldl")
      E.sig_type_decl_implicit

  let deriver =
    Type_conv.add name
      ~str_type_decl
      ~sig_type_decl

end
