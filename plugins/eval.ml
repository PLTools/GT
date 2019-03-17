open Base
open Ppxlib
open Printf

let trait_name = "eval"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

(* Should be renamed to gmap later *)
let trait_name = trait_name
open AstHelpers

class g initial_args tdecls = object(self: 'self)
  inherit G.g initial_args tdecls as super
  inherit P.with_inherit_arg initial_args tdecls as super2

  method trait_name = trait_name

  method! default_inh ~loc _tdecl = Typ.var ~loc "env"
  method inh_of_param tdecl _name =
    Typ.var ~loc:(loc_from_caml tdecl.ptype_loc) "env"

  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      let inh_t = self#default_inh ~loc tdecl in
      k @@ chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t)

  method! app_transformation_expr ~loc trf inh subj =
    Exp.app_list ~loc trf [ inh; subj ]

  method! plugin_class_params tdecl =
    let param_names,_,find_param,blownup_params = G.hack_params tdecl.ptype_params in
    let loc = loc_from_caml tdecl.ptype_loc in
    blownup_params @
    [ named_type_arg ~loc "env" ] @
    [ named_type_arg ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt
    ]

  method! prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      G.hack_params tdecl.ptype_params
    in
    let ps =
      List.concat_map rhs_args ~f:(fun t ->
          let open Ppxlib.Ast_builder.Default in
          [ t
          ; HelpersBase.map_core_type t
              ~onvar:(fun s -> Some (ptyp_var ~loc:t.ptyp_loc (find_param s)))
          ]
        )
    in
    (List.map ~f:Typ.from_caml ps) @
    [ Typ.var ~loc "env"]

  (* very similar as gmap but uses sgninfficant inherited attribute*)
  (* TODO: refactor somehow ??? *)
  method! on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.sprintf ~loc "env") @@
      Exp.fun_ ~loc pat @@
      Exp.record ~loc @@ List.map labs
        ~f:(fun {pld_name; pld_type} ->
            lident pld_name.txt,
            self#app_transformation_expr ~loc
              (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl pld_type)
              (Exp.ident ~loc "env")
              (Exp.ident ~loc pld_name.txt)
          )
    ]

end

let g =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
