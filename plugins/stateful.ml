open Base
open Ppxlib
open Printf

(* Should be renamed to gmap later *)
let trait_name = "stateful"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

let plugin_name = trait_name
open AstHelpers

class g initial_args = object(self: 'self)
  (* TODO: maybe do not inherit from gmap a.k.a. functor *)
  inherit G.g initial_args as super
  inherit [_] P.with_inherit_arg as super2

  method plugin_name = trait_name

  method! default_inh ~loc _tdecl = Typ.var ~loc "env"
  method! syn_of_param ~loc s =
    Typ.tuple ~loc [Typ.var ~loc "env"; Typ.var ~loc @@ Gmap.param_name_mangler s]
  method inh_of_param tdecl _name = Typ.var ~loc:(loc_from_caml tdecl.ptype_loc) "env"

  method! default_syn ~loc tdecl =
    Typ.tuple ~loc [self#default_inh ~loc tdecl; super#default_syn ~loc tdecl]

  method! make_typ_of_class_argument ~loc tdecl name k =
    k @@
    super#make_typ_of_class_argument ~loc tdecl name @@
    Typ.arrow ~loc (Typ.var ~loc "env")

  (* method! app_transformation_expr ~loc trf inh subj =
   *   Exp.app_list ~loc trf [ inh; subj ] *)

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

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info ts k =
    let names = List.map ts ~f:(fun _ -> gen_symbol ()) in
    let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
    k [
      Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.sprintf ~loc "env0") @@

      Exp.fun_list ~loc
        (List.map names ~f:(Pat.sprintf ~loc "%s"))
        (let c = match constr_info with
            | `Normal s -> Exp.construct ~loc (lident s)
            | `Poly s   -> Exp.variant ~loc s
         in
         match ts with
         | [] -> Exp.tuple ~loc [ Exp.ident ~loc "env0"; c [] ]
         | ts ->
           let res_var_name = sprintf "%s_rez" in
           let ts = List.zip_exn names ts in
           let ys = List.mapi ~f:(fun n x -> (n,x)) ts in
           List.fold_right ys
             ~init:(Exp.tuple ~loc [ Exp.sprintf ~loc "env%d" (List.length ys)
                                   ; c @@
                                     List.map ts
                                       ~f:(fun (n,t) -> Exp.ident ~loc @@ res_var_name n)
                                    ]
                   )
             ~f:(fun (i,(name,typ)) acc ->
                 Exp.let_one ~loc
                   (Pat.tuple ~loc [ Pat.sprintf ~loc "env%d" (i+1)
                                   ; Pat.sprintf ~loc "%s" @@ res_var_name name])
                   (self#app_transformation_expr ~loc
                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                     (Exp.sprintf ~loc "env%d" i)
                     (Exp.ident ~loc name)
                   )
                   acc
               )
       )
  ]

  method! on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    failwith "not implemented"
end

let g =  (new g :>
            (Plugin_intf.plugin_args ->
              (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
