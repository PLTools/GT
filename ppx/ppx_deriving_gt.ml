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
    Deriving.Args.(empty +> arg "options" r +> arg "plugins" r)
    (fun ~loc ~path info options plugins ->
      (* Expander.notify "with annotations %s" (String.concat "," info); *)
      let cfg =
        match options, plugins with
        | None, None -> None
        | Some p, None | None, Some p -> Some p
        | Some _, Some _ ->
          Location.raise_errorf
            ~loc
            "You can't specify both options and plugins. Use only one of them"
      in
      H.str_type_decl_many_plugins
        ~loc
        []
        (match cfg with
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
        info)
;;

let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make
    Deriving.Args.(empty +> arg "options" r +> arg "plugins" r)
    (fun ~loc ~path info options plugins ->
      let options =
        match options, plugins with
        | None, None -> None
        | Some p, None | None, Some p -> Some p
        | Some _, Some _ ->
          Location.raise_errorf
            ~loc
            "You can't specify both options and plugins. Use only one of them"
      in
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
                 | _ ->
                   Location.raise_errorf ~loc "only lowercase identifiers are allowed")
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
      let names = HelpersBase.vars_from_core_type typ |> HelpersBase.SS.elements in
      let tdecl =
        let open Ppxlib.Ast_builder.Default in
        type_declaration
          ~loc
          ~name:(Located.mk ~loc "dummy")
          ~params:
            (List.map
               (fun s -> ptyp_var ~loc s, (Asttypes.NoVariance, Asttypes.NoInjectivity))
               names)
          ~cstrs:[]
          ~private_:Public
          ~manifest:(Some typ)
          ~kind:Ptype_abstract
      in
      let rhs =
        p#do_typ_gen
          ~loc:(PpxHelpers.loc_from_caml loc)
          ~mutual_decls:[]
          ~is_self_rec:(fun _ -> `Nonrecursive)
          tdecl
          typ
      in
      let open Ppxlib.Ast_builder.Default in
      let pats, silence_warns =
        p#prepare_fa_args
          ~loc
          (fun ~loc ~flg ~pat ~expr ->
            pexp_let ~loc flg [ value_binding ~loc ~pat ~expr ])
          tdecl
      in
      List.fold_right (pexp_fun ~loc Nolabel None) pats (silence_warns rhs)
    in
    Deriving.add ~extension name |> Deriving.ignore)
;;

let () = Deriving.add ~str_type_decl ~sig_type_decl "gt" |> Deriving.ignore
