(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib

module E = Ppx_gt_expander

let gt_param name =
  let open Deriving.Args in
  arg name __

let str_type_decl : (_, _) Deriving.Generator.t =
  let bothp name rest =
    Deriving.Args.(rest +> (gt_param name) )
  in
  Deriving.Generator.make
    (* ~attributes:[ Attribute.T Attrs.ignore ] *)
    Deriving.Args.(empty
                   |> (bothp "show")
                   |> (bothp "gmap")
                   |> (bothp "foldl")
                   |> (bothp "show_typed")
                   |> (bothp "compare")
                   |> (bothp "eq")
                  )
    (fun ~loc ~path info show gmap foldl show_typed  compare eq  ->
      let wrap name = function
        | None -> E.Skip
        | Some e -> match e.pexp_desc with
          | Pexp_ident {txt} when Caml.(=) txt (Lident name) -> E.Use []
          | Pexp_record (ls, _) -> E.Use (List.map (fun (l,r) -> (l.txt,r)) ls)
          | _ -> HelpersBase.raise_errorf
                   "This kind of arguments is not supported. Bad expression %s"
                   (Pprintast.string_of_expression e)
      in
      let show       = wrap "show"       show in
      let gmap       = wrap "gmap"       gmap in
      let foldl      = wrap "foldl"      foldl in
      let show_typed = wrap "show_typed" show_typed in
      let compare    = wrap "compare"    compare in
      let eq         = wrap "eq"         eq in
      E.str_type_decl_implicit ~loc ~path
        info show gmap foldl compare eq show_typed
    )

let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make
    Deriving.Args.(empty
                   +> flag "show" +> flag "gmap" +> flag "foldl"
                   +> flag "compare" +> flag "eq" +> flag "show_typed"
                  )
    E.sig_type_decl_implicit

let () =
  (* Sys.command "notify-send 'Registering deriver' wtf" |> ignore; *)
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    "gt"
  |> Deriving.ignore
