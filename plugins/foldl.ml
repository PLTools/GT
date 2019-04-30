(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Foldl} plugin: fold all values in a type.

    Essentially is a stub that chains inherited attribute thorough all values
    in the value

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('s -> 'a -> 's) ->
     ('s -> 'b -> 's) ->
     ... ->
     's -> ('a,'b,...) typ -> 's ]
*)

open Base
open HelpersBase
open Ppxlib
open Printf

let trait_name = "foldl"

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers
module P = Plugin.Make(AstHelpers)

let trait_name =  trait_name
let make_dest_param_names ps =
  map_type_param_names ps ~f:(sprintf "%s_2")

class g initial_args tdecls = object(self: 'self)
  inherit P.with_inherit_arg initial_args tdecls as super

  method trait_name = trait_name

  method syn_of_param ~loc s = Typ.var ~loc "syn"
  method main_inh  ~loc tdecl = self#main_syn ~loc tdecl
  method main_syn  ~loc ?in_class tdecl = self#syn_of_param ~loc "dummy"

  method inh_of_param tdecl _ =
    self#syn_of_param ~loc:(loc_from_caml tdecl.ptype_loc) "dummy"

  method plugin_class_params tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    List.map tdecl.ptype_params ~f:(fun (t,_) -> typ_arg_of_core_type t) @
    [ named_type_arg ~loc "syn"
    ; named_type_arg ~loc @@
      Naming.make_extra_param tdecl.ptype_name.txt
    ]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    List.map rhs_args ~f:Typ.from_caml @
    [ self#main_syn ~loc tdecl
    ]

  method trf_scheme ~loc =
    Typ.(arrow ~loc (var ~loc "b") @@
         arrow ~loc (var ~loc "a") (var ~loc "b"))
  method trf_scheme_params = ["a";"b"]

  (* For foldl/r we specify this two methods manually *)
  method index_functor tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "Index_fold_%s" name
  method index_modtyp_name tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "IndexResult_fold_%s" name


  (* new type of trasfomation function is 'syn -> old_type *)
  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      let inh_t = self#inh_of_param tdecl name in
      k @@ chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t)


  (* method! make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
   *   let subj_t = Option.value subj_t ~default:(Typ.use_tdecl tdecl) in
   *   let syn_t  = Option.value syn_t  ~default:(self#default_syn ~loc tdecl) in
   *   Typ.arrow ~loc (self#default_inh ~loc tdecl)
   *     (super#make_RHS_typ_of_transformation ~loc ~subj_t ~syn_t tdecl) *)

  method join_args ~loc do_typ ~init (xs: (string * core_type) list) =
    List.fold_left ~f:(fun acc (name,typ) ->
        Exp.app_list ~loc
          (do_typ typ)
          [ acc; Exp.sprintf ~loc "%s" name]
        )
        ~init
        xs

  method on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl constr_info args =
    let names = List.map args ~f:fst in
    Exp.fun_list ~loc (List.map names ~f:(Pat.sprintf ~loc "%s")) @@
    self#join_args ~loc (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl)
        ~init:inhe
        args

  method on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_list ~loc
        [ Pat.sprintf ~loc "inh"; pat]
        (Exp.failwith_ ~loc "not implemented. TODO")

    ]

end

let create =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
