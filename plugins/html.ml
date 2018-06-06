(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(* NOT IMPELEMENTED YET *)
open Base
open Ppxlib
open HelpersBase
open Printf

let trait_name = "html"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let plugin_name = trait_name

module P = Plugin.Make(AstHelpers)
open AstHelpers

let app_format_sprintf ~loc arg =
  Exp.app ~loc
    (Exp.of_longident ~loc (Ldot(Lident "Format", "sprintf")))
    arg

class g args = object(self)
  inherit [loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args
  inherit P.no_inherit_arg

  method plugin_name = trait_name
  method default_inh ~loc _tdecl = Typ.ident ~loc "unit"
  method default_syn ~loc _tdecl = self#syn_of_param ~loc "dummy"

  method syn_of_param ~loc _     = Typ.of_longident ~loc (Ldot (Lident "View", "viewer"))
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

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    match bindings with
    | [] -> k @@ Exp.string_const ~loc ("`"^constr_name)
    | _ ->
      k @@ List.fold_left
        bindings
        ~f:(fun acc (name, typ) -> Exp.app ~loc acc
               (Exp.app ~loc
                  (self#do_typ_gen ~loc ~mutal_names ~is_self_rec typ)
                  (Exp.ident ~loc name))
           )
        ~init:Exp.(app ~loc
                     (of_longident ~loc (Ldot(Lident "Format", "sprintf"))) @@

                   let fmt = String.concat ~sep:", " @@ List.map bindings
                       ~f:(fun _ -> "%s")
                   in
                   Exp.string_const ~loc @@ Printf.sprintf "`%s(%s)" constr_name fmt
                  )


  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info ts k =
    k @@
    [ let methname = sprintf "c_%s" (match constr_info with
            `Normal s -> s | `Poly s -> s) in
      (* let constr_name = match constr_info with
       *   | `Poly s -> sprintf "`%s" s
       *   | `Normal s -> s
       * in *)
      Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.unit ~loc) @@

        let names = List.map ts ~f:(fun _ -> gen_symbol ()) in
        Exp.fun_list ~loc
          (List.map names ~f:(Pat.sprintf ~loc "%s"))
          (Exp.assert_false ~loc)
  ]


  method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    []
end

let g = (new g :> (Plugin_intf.plugin_args ->
                   (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
