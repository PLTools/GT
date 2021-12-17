(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib
open GTCommon
module E = Expander.Make (PpxHelpers)

let gt_param name =
  let open Deriving.Args in
  arg name __
;;

let r =
  let open Deriving.Args in
  map1 ~f:(List.map (fun ({ txt }, e) -> txt, e)) @@ pexp_record __ none
;;

module H = Expander.Make (PpxHelpers)

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make
    Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
      (* Expander.notify "with annotations %s" (String.concat "," info); *)
      let generator_f si =
        H.str_type_decl_many_plugins
          ~loc
          si
          (match options with
          | None -> []
          | Some xs ->
            List.map
              (function
                | Lident name, e ->
                  let extra =
                    match e.pexp_desc with
                    | Pexp_record (xs, _) -> List.map (fun ({ txt }, b) -> txt, b) xs
                    | Pexp_ident { txt = Lident s } when s = name -> []
                    | _ -> Location.raise_errorf ~loc "bad argument of a plugin"
                  in
                  name, Expander.Use extra
                | _ -> Location.raise_errorf ~loc "only lowercase identifiers are allowed")
              xs)
      in
      generator_f [] info)
;;

let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make
    Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
      let generator_f si =
        H.sig_type_decl_many_plugins
          ~loc
          si
          (match options with
          | None -> []
          | Some xs ->
            List.map
              (function
                | Lident name, e -> name, Expander.Use []
                | _ -> Location.raise_errorf ~loc "only lowercase identifiers are allowed")
              xs)
      in
      generator_f [] info)
;;

let () =
  Expander.set_inline_registration (fun name (module M : Plugin_intf.MAKE) ->
      let module P = M (PpxHelpers) in
      let p =
        let loc = Location.none in
        let dummy_decl =
          match [%stri type nonrec t = int] with
          | { pstr_desc = Pstr_type (_, x) } -> x
          | _ -> Location.raise_errorf ~loc "Should not happen %s %d" __FILE__ __LINE__
        in
        P.create [] (false, dummy_decl)
      in
      let extension ~loc ~path:_ typ =
        let tdecl =
          let open Ppxlib.Ast_builder.Default in
          type_declaration
            ~loc
            ~name:(Located.mk ~loc "dummy")
            ~params:[]
            ~cstrs:[]
            ~private_:Public
            ~manifest:(Some typ)
            ~kind:Ptype_abstract
        in
        p#do_typ_gen
          ~loc:(PpxHelpers.loc_from_caml loc)
          ~mutual_decls:[]
          ~is_self_rec:(fun _ -> `Nonrecursive)
          tdecl
          typ
      in
      Deriving.add ~extension name |> Deriving.ignore)
;;

let () = Deriving.add ~str_type_decl ~sig_type_decl "gt" |> Deriving.ignore
