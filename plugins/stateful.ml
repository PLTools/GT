(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Stateful} plugin: functors + inherited value
    to make decisions about how to map values.

    Behave the same as {!Eval} trait but can may return modified state.

    Inherited attributes' type (both default and for type parameters) is ['env].

    Synthetized attributes' type (both default and for type parameters) is ['env * _ t].

    For type declaration [type ('a,'b,...) typ = ...] it will create transformation
    function with type

    [('env -> 'a -> 'env * 'a2) ->
     ('env -> 'b -> 'env * 'b2) -> ... ->
     'env -> ('a,'b,...) typ -> 'env * ('a2, 'b2, ...) typ ]

  *)

open Base
open Ppxlib
open Printf
open HelpersBase

let trait_name = "stateful"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

let trait_name = trait_name
open AstHelpers

class g initial_args tdecls = object(self: 'self)
  (* TODO: maybe do not inherit from gmap a.k.a. functor *)
  inherit G.g initial_args tdecls as super
  inherit P.with_inherit_arg initial_args tdecls as super2

  method trait_name = trait_name

  method! main_inh ~loc _tdecl = Typ.var ~loc "env"
  method! syn_of_param ~loc s =
    Typ.tuple ~loc [Typ.var ~loc "env"; Typ.var ~loc @@ Gmap.param_name_mangler s]
  method inh_of_param tdecl _name = Typ.var ~loc:(loc_from_caml tdecl.ptype_loc) "env"

  method! main_syn ~loc ?in_class tdecl =
    let in_class = match in_class with
      | None -> false
      | Some b -> b
    in
    Typ.tuple ~loc [self#main_inh ~loc tdecl; super#main_syn ~loc ~in_class tdecl]

  method trf_scheme_params = ["env"; "a"; "b"]
  method! trf_scheme ~loc =
    let v s =
      assert (List.mem ~equal:String.equal self#trf_scheme_params s);
      Typ.var ~loc s
    in
    Typ.(arrow ~loc (v "env") @@
         arrow ~loc (v "a") @@
         pair ~loc (v "env") (v "b") )

  method! plugin_class_params tdecl =
    super#plugin_class_params tdecl @
    [named_type_arg ~loc:(loc_from_caml tdecl.ptype_loc) "env"]

  method! prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    super#prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args @
    [ Typ.var ~loc "env"]

  method! extra_class_sig_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    let wrap =
      if is_polyvariant_tdecl tdecl
      then Typ.openize ~loc
      else (fun ?as_ x -> x)
    in
    [ Ctf.constraint_ ~loc
        (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
        (wrap @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
         map_type_param_names tdecl.ptype_params
           ~f:(fun s -> Typ.var ~loc s)
        )
    ]

  method! extra_class_str_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    let wrap =
      if is_polyvariant_tdecl tdecl
      then Typ.openize ~loc
      else (fun ?as_ x -> x)
    in
    [ Cf.constraint_ ~loc
        (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
        (wrap @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
         map_type_param_names tdecl.ptype_params
           ~f:(fun s -> Typ.var ~loc s)
        )
    ]

  method on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl constr_info ts =
      Exp.fun_list ~loc
        (List.map ts ~f:(fun p -> Pat.sprintf ~loc "%s" @@ fst p))
        (let c = match constr_info with
            | `Normal s -> Exp.construct ~loc (lident s)
            | `Poly s   -> Exp.variant ~loc s
         in
         match ts with
         | [] -> Exp.tuple ~loc [ inhe; c [] ]
         | ts ->
           let res_var_name = sprintf "%s_rez" in
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
                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl typ)
                     (if i=0 then inhe else Exp.sprintf ~loc "env%d" i)
                     (Exp.ident ~loc name)
                   )
                   acc
               )
       )

  method! on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl labs =
    (* TODO: *)
    failwith "not implemented"
end

let create =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
