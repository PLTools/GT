(** {i Gmap} plugin (functor).

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('a -> 'a2) -> ('b -> 'b2) -> ... -> ('a,'b,...) typ -> ('a2,'b2,...) typ ]

    Inherited attributes' type (both default and for type parameters) is [unit].
*)

(*
 * Generic transformers (GT): `hash` plugin.
 * Copyright (C) 2017-2019
 *   Dmitrii Kosarev a.k.a. Kakadu
 * St.Petersburg University, JetBrains Research
 *)

open Base
open HelpersBase
open Ppxlib
open Printf
open Ast_helper

let trait_name = "gmap"
let param_name_mangler = sprintf "%s_2"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name
module P = Plugin.Make(AstHelpers)
open AstHelpers

(* TODO: rethink this function *)
let hack_params ?(loc=noloc) ps =
  let param_names = map_type_param_names ps ~f:id in
  let rez_names = map_type_param_names ps ~f:param_name_mangler in
  let name_migrations = List.zip_exn param_names rez_names in
  let assoc s =
    try List.Assoc.find_exn ~equal:String.equal name_migrations s
    with Caml.Not_found ->
      raise_errorf "can't find new typ for param `%s" s
  in
  let blownup_params =
    List.concat_map param_names
      ~f:(fun s1 ->
           [named_type_arg ~loc s1; named_type_arg ~loc @@ assoc s1 ]
         )
  in
  (param_names, rez_names, assoc, blownup_params)

class g args tdecls = object(self: 'self)
  inherit P.no_inherit_arg args tdecls

  method trait_name = trait_name

  method default_inh ~loc _tdecl = Typ.ident ~loc "unit"
  method syn_of_param ~loc s = Typ.var ~loc @@ param_name_mangler s
  method inh_of_param tdecl _name =
    self#default_inh ~loc:(loc_from_caml tdecl.ptype_loc) tdecl

  method default_syn ~loc ?(in_class=false) tdecl =
    if in_class && is_polyvariant_tdecl tdecl then
      Typ.var ~loc @@ sprintf "extra_%s" tdecl.ptype_name.txt
    else
    let param_names,rez_names,find_param,blownup_params =
      hack_params tdecl.ptype_params
    in
    let ans =
      let ident = Lident (self#cur_name tdecl) in
      Typ.constr ~loc ident @@
      List.map ~f:(Typ.var ~loc) rez_names
    in
    ans

  method plugin_class_params tdecl =
    let param_names,_,find_param,blownup_params = hack_params tdecl.ptype_params in
    blownup_params @
    [ named_type_arg ~loc:(loc_from_caml tdecl.ptype_loc) @@
      Naming.make_extra_param tdecl.ptype_name.txt
    ]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      hack_params tdecl.ptype_params
    in
    let ps =
      List.concat_map rhs_args ~f:(fun t ->
          let open Ppxlib.Ast_builder.Default in
          [ t
          ; map_core_type t ~onvar:(fun s -> Some (ptyp_var ~loc:t.ptyp_loc (find_param s)))]
        )
    in
    List.map ~f:Typ.from_caml ps

  method trf_scheme ~loc =
    Typ.(arrow ~loc (unit ~loc) @@
         arrow ~loc (var ~loc "a") (var ~loc "b"))
  method trf_scheme_params = ["a"; "b"]

  method! extra_class_sig_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    if not (is_polyvariant_tdecl tdecl) then [] else
      [ Ctf.constraint_ ~loc
          (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
          (Typ.openize ~loc @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
           map_type_param_names tdecl.ptype_params
             ~f:(fun s -> Typ.var ~loc @@ param_name_mangler s)
          )
      ]

  method! extra_class_str_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    if not (is_polyvariant_tdecl tdecl) then [] else
      [ Cf.constraint_ ~loc
          (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
          (Typ.openize ~loc @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
           map_type_param_names tdecl.ptype_params
             ~f:(fun s -> Typ.var ~loc @@ param_name_mangler s)
          )
      ]



  method on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl constr_info ts =
    Exp.fun_list ~loc
      (List.map ts ~f:(fun p -> Pat.sprintf ~loc "%s" @@ fst p))
      (let ctuple =
         List.map ts
           ~f:(fun (name, typ) ->
               self#app_transformation_expr ~loc
                 (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl typ)
                 inhe
                 (Exp.ident ~loc name)
             )
       in
       (match constr_info with
        | `Normal s -> Exp.construct ~loc (lident s) ctuple
        | `Poly s   ->
          let ans =  Exp.variant ~loc s ctuple in
          ans
       )
      )


  method on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.unit ~loc) @@
      Exp.fun_ ~loc pat @@
      Exp.record ~loc @@ List.map labs
        ~f:(fun {pld_name; pld_type} ->
            lident pld_name.txt,
            self#app_transformation_expr ~loc
              (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl pld_type)
              (Exp.unit ~loc)
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
