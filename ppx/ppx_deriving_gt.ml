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
(* module Attrs     = Ppx_gt_expander.Attrs *)

module Sexp_of = struct
  module E = Ppx_gt_expander
  let name = "gt"

  let str_type_decl : (_, _) Type_conv.Generator.t =
    Type_conv.Generator.make
      Type_conv.Args.(empty +> flag "show" +> flag "gmap")
      E.str_type_decl
      (* ~attributes:[ Attribute.T Attrs.default
       *             ; Attribute.T Attrs.drop_default
       *             ; Attribute.T Attrs.drop_if
       *             ] *)

  let deriver =
    Type_conv.add name
      ~str_type_decl
      (* ~sig_type_decl *)


  (* let () =
   *   Ppx_driver.register_transformation name
   *     ~rules:[ Context_free.Rule.extension
   *                (Extension.declare name
   *                   Core_type Ast_pattern.(ptyp __)
   *                   (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
   *            ] *)
end

