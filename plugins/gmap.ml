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
  inherit P.no_inherit_arg args tdecls as super

  method trait_name = trait_name

  method main_inh ~loc _tdecl = Typ.ident ~loc "unit"
  method syn_of_param ~loc s = Typ.var ~loc @@ param_name_mangler s
  method inh_of_param tdecl _name =
    self#main_inh ~loc:(loc_from_caml tdecl.ptype_loc) tdecl

  method main_syn ~loc ?(in_class=false) tdecl =
    if in_class && is_polyvariant_tdecl tdecl then
      Typ.var ~loc @@ sprintf "syn_%s" tdecl.ptype_name.txt
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
    ; named_type_arg ~loc:(loc_from_caml tdecl.ptype_loc) @@
      sprintf "syn_%s" tdecl.ptype_name.txt
    ]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    let _param_names,_rez_names,find_param,_blownup_params =
      hack_params tdecl.ptype_params
    in
    let ps =
      List.concat_map rhs_args ~f:(fun t ->
          let open Ppxlib.Ast_builder.Default in
          [ t
          ; map_core_type t ~onvar:(fun s -> Some (ptyp_var ~loc:t.ptyp_loc (find_param s)))
          ]
        )
    in
    List.map ~f:Typ.from_caml ps @
    [ Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt
    ; Typ.var ~loc @@ Printf.sprintf "syn_%s" tdecl.ptype_name.txt
    ]

  method hack ~loc (mangler: string -> string) param tdecl: Typ.t =
    let loc = loc_from_caml tdecl.ptype_loc in
    let on_abstract () =
      Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
      map_type_param_names tdecl.ptype_params ~f:(fun s -> Typ.var ~loc @@ mangler s)
    in
    visit_typedecl ~loc tdecl
      ~onopen:(fun () -> failwith "open types are not supported")
      ~onrecord:(fun _ -> on_abstract ())
      ~onvariant:(fun _ -> on_abstract ())
      ~onabstract:(fun () -> on_abstract ())
      ~onmanifest:(fun typ ->
          if not (is_polyvariant typ) then on_abstract ()
          else match typ.ptyp_desc with
            | Ptyp_variant (rf,_,_) ->
              let onvar s =
                let open Ast_builder.Default in
                Some (ptyp_var ~loc:tdecl.ptype_loc @@ mangler s)
              in

              Typ.variant ~loc ~is_open:true @@
              List.map rf ~f:(fun rf -> match rf.prf_desc with
                  | (Rtag (name,has_empty, ts)) ->
                    let open Ast_builder.Default in
                    let rec on_t t = map_core_type
                        ~onvar
                        ~onconstr:(fun name ts ->
                            match name with
                            | Lident s when String.equal s tdecl.ptype_name.txt ->
                              Option.some @@ ptyp_var ~loc:tdecl.ptype_loc param
                            | _  -> None
                          )
                        t
                    in
                    {rf with prf_desc = Rtag (name, has_empty, List.map ts ~f:on_t) }
                  | Rinherit typ ->
                    {rf with prf_desc = Rinherit (map_core_type typ ~onvar) }
                )
            | _ -> failwith "should not happen"
        )

  method! extra_class_sig_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    (* let wrap =
     *   if is_polyvariant_tdecl tdecl
     *   then Typ.openize ~loc
     *   else (fun ?as_ x -> x)
     * in *)
    (super#extra_class_sig_members tdecl) @
    [ let syn = sprintf "syn_%s" tdecl.ptype_name.txt in
      Ctf.constraint_ ~loc
        (Typ.var ~loc @@ syn)
        (self#hack ~loc param_name_mangler syn tdecl)
    ]

  method! extra_class_str_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    (super#extra_class_str_members tdecl) @
    [ let syn = sprintf "syn_%s" tdecl.ptype_name.txt in
      Cf.constraint_ ~loc
        (Typ.var ~loc @@ syn)
        (self#hack ~loc param_name_mangler syn tdecl)
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

  method! on_record_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl info bindings labs =
    assert Int.(List.length labs > 0);
    let is_poly,cname =
      match info with
      | `Normal s -> false,  s
      | `Poly   s -> true,   s
    in
    Exp.fun_list ~loc (List.map bindings ~f:(fun (s,_,_) -> Pat.sprintf ~loc "%s" s)) @@
    (if is_poly then Exp.variant ~loc cname
    else Exp.construct ~loc (lident cname))
      [Exp.record ~loc @@ List.map bindings ~f:(fun (s,labname,typ) ->
        ( lident labname
        , self#app_transformation_expr ~loc
                  (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl typ)
                  inhe
                  (Exp.ident ~loc s)
        )
      )
      ]


end

let create =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()
