(*
 * Generic Transformers: `format` plugin.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib
open HelpersBase
open Printf

let trait_name = "fmt"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let plugin_name = trait_name

module P = Plugin.Make(AstHelpers)
open AstHelpers

let app_format_fprintf ~loc efmtr efmts =
  Exp.app_list ~loc
    Exp.(of_longident ~loc (Ldot(Lident "Format", "fprintf")) )
    [ efmtr; efmts ]

class g args = object(self)
  inherit [loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args
  inherit P.with_inherit_arg

  method plugin_name = trait_name
  method default_inh ~loc _tdecl =
    Typ.of_longident ~loc (Ldot (Lident"Format", "formatter"))
  method default_syn ~loc  ?extra_path  _tdecl = Typ.ident ~loc "unit"

  method syn_of_param ~loc _     = Typ.ident ~loc "unit"
  method inh_of_param tdecl _name = self#default_inh ~loc:noloc tdecl

  method plugin_class_params tdecl =
    (* TODO: reuse prepare_inherit_typ_params_for_alias here *)
    let ps =
      List.map tdecl.ptype_params ~f:(fun (t,_) -> typ_arg_of_core_type t)
    in
    ps @
    [ named_type_arg ~loc:(loc_from_caml tdecl.ptype_loc) Plugin.extra_param_name]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    List.map rhs_args ~f:Typ.from_caml @
    [ Typ.var ~loc Plugin.extra_param_name]

  (* Adapted to generate only single method per constructor definition *)
  method on_tuple_constr ~loc ~is_self_rec ~mutal_names ~inhe constr_info ts =
    let constr_name = match constr_info with
      | `Poly s -> sprintf "`%s" s
      | `Normal s -> s
    in

    (* let names = List.map ts ~f:fst in *)
    let fmt = List.map ts ~f:(fun _ -> "%a") |> String.concat ~sep:",@,@ " in
    let fmt = sprintf "%s@ @[(@,%s@,)@]" constr_name fmt in

    Exp.fun_list ~loc
      (List.map ts ~f:(fun (s,_) -> Pat.sprintf ~loc "%s" s))
      (if List.length ts = 0
       then app_format_fprintf ~loc inhe @@ Exp.string_const ~loc constr_name
       else
         List.fold_left ts
           ~f:(fun acc (name, typ) ->
                Exp.app_list ~loc acc
                  [ self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ
                  ; Exp.ident ~loc name
                  ]
             )
            ~init:(app_format_fprintf ~loc inhe @@
                   Exp.string_const ~loc fmt
                  )
      )

  method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    let fmt = List.fold_left labs ~init:""
        ~f:(fun acc x ->
            sprintf "%s@,@ @,@[%s@,=@,%%a;@]" acc x.pld_name.txt
          )
    in
    let fmt_name = gen_symbol ~prefix:"fmt" () in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.sprintf "%s" ~loc fmt_name) @@
      Exp.fun_ ~loc pat @@
      List.fold_left labs
            ~f:(fun acc {pld_name; pld_type} ->
                Exp.app_list ~loc acc
                  [ self#do_typ_gen ~loc ~is_self_rec ~mutal_names pld_type
                  ; Exp.ident ~loc pld_name.txt
                  ]
              )
            ~init:(app_format_fprintf ~loc (Exp.sprintf "%s" ~loc fmt_name) @@
                   Exp.string_const ~loc @@ sprintf "{@[<hov>%s@]@ }@," fmt
                  )
    ]

end

let g = (new g :> (Plugin_intf.plugin_args ->
                   (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
