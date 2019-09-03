(** {i Compare} plugin: receive another value as inherited attribute and compare.

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('a -> 'a -> GT.comparison) ->
     ('b -> 'b -> GT.comparison) -> ... -> ('a,'b,...) typ -> GT.comparison ]

    Inherited attribute' is the same as argument, synthetized attribute is {!GT.comparison}.
*)

(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev a.k.a. Kakadu
 * St.Petersburg University, JetBrains Research
 *)
open Base
open Ppxlib
open HelpersBase
open Printf

let trait_name = "compare"

(* Compare plugin where we pass another value of the same type as 'inh
 * and return GT.comparison as 'syn
 *)
module Make(AstHelpers : GTHELPERS_sig.S) = struct

module P = Plugin.Make(AstHelpers)
open AstHelpers

let trait_name = trait_name

let access_GT s = Ldot (Lident "GT", s)

class g initial_args tdecls = object(self: 'self)
  inherit P.with_inherited_attr initial_args tdecls

  method trait_name = trait_name

  method inh_of_main ~loc tdecl =
    let ans = Typ.use_tdecl tdecl in
    if is_polyvariant_tdecl tdecl
    then Typ.alias ~loc (Typ.variant_of_t ~loc ans) @@
      Naming.make_extra_param tdecl.ptype_name.txt
    else ans

  method syn_of_param ~loc _s = Typ.of_longident ~loc (Ldot (Lident "GT", "comparison"))
  method syn_of_main  ~loc ?in_class tdecl = self#syn_of_param ~loc "dummy"

  method inh_of_param ~loc _tdecl name = Typ.var ~loc name

  method plugin_class_params tdecl =
    List.map tdecl.ptype_params ~f:(fun (t,_) -> typ_arg_of_core_type t) @
    [ named_type_arg ~loc:(loc_from_caml tdecl.ptype_loc) @@
      Naming.make_extra_param tdecl.ptype_name.txt
    ]

  method alias_inherit_type_params ~loc tdecl rhs_args =
    List.map rhs_args ~f:Typ.from_caml @
    [ Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt ]


  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      let inh_t = subj_t in
      k @@ chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t)

  method chain_exprs ~loc e1 e2 =
    Exp.app_list ~loc
      (Exp.of_longident ~loc (access_GT "chain_compare"))
      [ e1
      ; Exp.fun_ ~loc (Pat.unit ~loc) e2
      ]
  (* [%expr GT.chain_compare [%e e1] (fun () -> [%e e2]) ] *)

  method chain_init ~loc = Exp.construct ~loc (access_GT "EQ") []

  method on_different_constructors ~loc is_poly other_name cname arg_typs =
    Exp.app_list ~loc
      (Exp.of_longident ~loc (access_GT "compare_vari"))
      [ Exp.ident ~loc other_name
      ; ( if is_poly
          then Exp.variant ~loc cname
          else Exp.construct ~loc (lident cname)) @@
        (match arg_typs with
        | `Tuples ts -> List.map ts ~f:(fun _ -> Exp.objmagic_unit ~loc)
        | `Record rs ->
            [Exp.record ~loc @@ List.map rs ~f:(fun (_,l,_) -> (lident l,Exp.objmagic_unit ~loc))]
        )
        (* List.map arg_typs ~f:(fun _ -> Exp.objmagic_unit ~loc) *)
        (* It's annoying to use magic here but need to do this first:
           https://caml.inria.fr/mantis/print_bug_page.php?bug_id=4751
        *)
      ]

  method on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl constr_info args =
    let is_poly,cname =
      match constr_info with
      | `Normal s -> false,  s
      | `Poly   s -> true,   s
    in

        let main_case =
          let pat_names = List.map args ~f:(fun _ -> gen_symbol ()) in
          let lhs =
            let arg_pats =
              match pat_names with
              | []  -> []
              | [s] -> [Pat.var ~loc s]
              | __  -> List.map pat_names ~f:(Pat.var ~loc)
            in
            if is_poly
            then Pat.variant   ~loc  cname arg_pats
            else Pat.constr    ~loc  cname arg_pats
          in
          let rhs =
            List.fold_left  ~init:(self#chain_init ~loc)
              (List.map2_exn pat_names args ~f:(fun a (b,c) -> (a,b,c)))
              ~f:(fun acc (pname, name, typ) ->
                self#chain_exprs ~loc
                  acc
                  (self#app_transformation_expr ~loc
                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl typ)
                     (Exp.ident ~loc pname)
                     (Exp.ident ~loc name)
                  )
              )
          in
          case ~lhs ~rhs
        in

        let other_case =
          let other_name = "other" in
          let lhs = Pat.var ~loc other_name in
          let rhs =
            self#on_different_constructors ~loc is_poly other_name cname (`Tuples args)
          in
          case ~lhs ~rhs
        in

        Exp.fun_list ~loc (List.map args ~f:(fun (s,_) -> Pat.sprintf ~loc "%s" s)) @@
        Exp.match_ ~loc inhe [ main_case; other_case ]

  method app_transformation_expr ~loc trf inh subj =
    Exp.app_list ~loc trf [ inh; subj ]

  method abstract_trf ~loc k =
    Exp.fun_list ~loc [Pat.sprintf ~loc "inh"; Pat.sprintf ~loc "subj"] @@
    k (Exp.sprintf ~loc "inh") (Exp.sprintf ~loc "subj")
    (* [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]] *)

  method on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl labs =
    assert Int.(List.length labs > 0);
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun {pld_name} ->
          (Lident pld_name.txt, Pat.var ~loc pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      (* TODO: maybe use abstract_transformation_expr here *)
      Exp.fun_list ~loc
        [ Pat.sprintf ~loc "inh"; pat] @@
        let wrap lab =
          self#app_transformation_expr ~loc
            (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl lab.pld_type)
            (Exp.field ~loc (Exp.ident ~loc "inh") (Lident lab.pld_name.txt))
            (Exp.ident ~loc lab.pld_name.txt )
        in
        let init = self#chain_init ~loc in
        List.fold_left ~init labs
          ~f:(fun acc lab -> self#chain_exprs ~loc acc (wrap lab))
    ]

  method! on_record_constr ~loc ~is_self_rec ~mutal_decls ~inhe tdecl info bindings labs =
    assert Int.(List.length labs > 0);
    let is_poly,cname =
      match info with
      | `Normal s -> false,  s
      | `Poly   s -> true,   s
    in
    let main_case =
      let pat_names = List.map labs ~f:(fun _ -> gen_symbol ()) in
      let lhs =
        let arg_pats =
          match pat_names with
          | []  -> []
          | [s] -> [Pat.var ~loc s]
          | __  -> List.map pat_names ~f:(Pat.var ~loc)
        in
        Pat.constr ~loc cname
          [ Pat.record ~loc @@ List.map2_exn labs pat_names ~f:(fun l name ->
              (lident l.pld_name.txt, Pat.var ~loc name) )
          ]
      in
      let rhs =
        List.fold_left ~init:(self#chain_init ~loc)
          (List.map2_exn bindings pat_names ~f:(fun (name1,_,typ) iname -> (name1,iname,typ)))
          ~f:(fun acc (sname, iname, typ) ->
            self#chain_exprs ~loc
              acc
              (self#app_transformation_expr ~loc
                  (self#do_typ_gen ~loc ~is_self_rec ~mutal_decls tdecl typ)
                  (Exp.ident ~loc iname)
                  (Exp.ident ~loc sname)
              )
          )
      in
      case ~lhs ~rhs
    in
    let other_case =
      let other_name = "other" in
      let lhs = Pat.var ~loc other_name in
      let rhs =
        self#on_different_constructors ~loc is_poly other_name cname @@
        `Record bindings
      in
      case ~lhs ~rhs
    in
    Exp.fun_list ~loc (List.map bindings ~f:(fun (s,_,_) -> Pat.sprintf ~loc "%s" s)) @@
    Exp.match_ ~loc inhe [ main_case; other_case ]

end

let create =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()
