open Base
open Ppxlib
open Printf

(* Should be renamed to gmap later *)
let trait_name = "eval"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

let plugin_name = trait_name
open AstHelpers

class g initial_args = object(self: 'self)
  (* inherit P.generator initial_args as super *)
  inherit G.g initial_args as super
  inherit [_] P.with_inherit_arg as super2

  method plugin_name = trait_name

  method! default_inh ~loc _tdecl = Typ.var ~loc "env"
  method inh_of_param tdecl _name = Typ.var ~loc:(loc_from_caml tdecl.ptype_loc) "env"


  method! make_typ_of_class_argument ~loc tdecl name k =
    k @@
    super#make_typ_of_class_argument ~loc tdecl name @@
    Typ.arrow ~loc (Typ.var ~loc "env")

  method! app_transformation_expr ~loc trf inh subj =
    (* assert false; *)
    Exp.app_list ~loc trf [ inh; subj ]

  method! plugin_class_params tdecl =
    let param_names,_,find_param,blownup_params = G.hack_params tdecl.ptype_params in
    let loc = loc_from_caml tdecl.ptype_loc in
    blownup_params @
    [named_type_arg ~loc "env"] @
    [named_type_arg ~loc Plugin.extra_param_name]

  method! prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      G.hack_params tdecl.ptype_params
    in
    let ps =
      List.concat_map rhs_args ~f:(fun t ->
          let open Ppxlib.Ast_builder.Default in
          [ t
          ; HelpersBase.map_core_type t
              ~onvar:(fun s -> ptyp_var ~loc:t.ptyp_loc (find_param s))
          ]
        )
    in
    (List.map ~f:Typ.from_caml ps) @
    [ Typ.var ~loc "env"] @
    [ Typ.var ~loc Plugin.extra_param_name ]


end

let g =  (new g :>
            (Plugin_intf.plugin_args ->
              (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
