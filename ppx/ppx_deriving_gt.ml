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

module Sexp_of = struct
  module E = Ppx_gt_expander
  let name = "gt"

  let str_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      Type_conv.Args.(empty +> flag "show" +> flag "gmap" +> flag "foldl")
      E.str_type_decl_implicit

  let deriver =
    Type_conv.add name
      ~str_type_decl
      (* ~sig_type_decl *)

end
