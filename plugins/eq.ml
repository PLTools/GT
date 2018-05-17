open Base
open Ppxlib
open Printf

let trait_name = "eq"
(* Compare plugin where we pass another value of the same type as 'inh
 * and return true or false
*)

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module C = Compare.Make(AstHelpers)
let plugin_name = trait_name
open AstHelpers

class g initial_args = object(self: 'self)
  inherit C.g initial_args as super

  method plugin_name = trait_name

  (* method default_inh = core_type_of_type_declaration *)
  method syn_of_param ~loc s = Typ.sprintf ~loc "bool"
  method default_syn ~loc tdecl = self#syn_of_param ~loc "dummy"

  (* method inh_of_param tdecl name =
   *   let loc = tdecl.ptype_loc in
   *   Typ.var ~loc name
   *
   * method plugin_class_params tdecl =
   *   List.map ~f:fst tdecl.ptype_params
   *
   * method prepare_inherit_args_for_alias ~loc tdecl rhs_args =
   *   rhs_args (\* @ [ self#default_syn tdecl ] *\)
   *              (\* TODO: add 'extra ? *\) *)

  (* method! make_typ_of_self_trf ~loc tdecl =
   *   [%type: [%t self#default_inh tdecl] ->
   *     [%t super#make_typ_of_self_trf ~loc tdecl] ]
   *
   * method make_typ_of_class_argument ~loc tdecl name =
   *   [%type: [%t self#default_inh tdecl] ->
   *     [%t super#make_typ_of_class_argument ~loc tdecl name] ] *)

  (* method! make_RHS_typ_of_transformation ?subj_t ?syn_t tdecl =
   *   let loc = tdecl.ptype_loc in
   *   let subj_t = Option.value subj_t
   *       ~default:(using_type ~typename:tdecl.ptype_name.txt tdecl) in
   *   let syn_t  = Option.value syn_t  ~default:(self#default_syn tdecl) in
   *   [%type: [%t self#default_inh tdecl] ->
   *     [%t super#make_RHS_typ_of_transformation ~subj_t ~syn_t tdecl] ] *)

  (* method wrap_tr_function_str ~loc _tdelcl  make_gcata_of_class =
   *   let body = make_gcata_of_class [%expr self] in
   *   [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj] *)

  method! on_different_constructors ~loc is_poly other_name cname arg_typs =
    Exp.false_ ~loc
  method chain_exprs ~loc e1 e2 =
    Exp.app_list ~loc (Exp.ident ~loc "&&") [ e1; e2 ]
    (* [%expr  [%e e1] && [%e e2] ] *)
  method chain_init ~loc = Exp.true_ ~loc
    (* [%expr true ] *)

  (* method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
   *   let is_poly,cname =
   *     match constr_info with
   *     | `Normal s -> false,  s
   *     | `Poly   s -> true,   s
   *   in
   *   let methname = sprintf "c_%s" cname in
   *   let names     = List.map args ~f:(fun _ -> gen_symbol ()) in
   *
   *   k @@ [
   *   Cf.method_concrete ~loc methname
   *     [%expr fun inh -> [%e
   *       let main_case =
   *         let pat_names = List.map args ~f:(fun _ -> gen_symbol ()) in
   *         let lhs =
   *           let arg_pats =
   *             match pat_names with
   *             | []  -> None
   *             | [s] -> Some (Pat.var ~loc s)
   *             | __  ->
   *                 Some (Pat.tuple ~loc @@ List.map pat_names ~f:(Pat.var ~loc))
   *           in
   *           if is_poly
   *           then Pat.variant   ~loc  cname         arg_pats
   *           else Pat.construct ~loc (lident cname) arg_pats
   *         in
   *         let rhs =
   *           List.fold_left  ~init:[%expr GT.EQ]
   *             (List.map3_exn pat_names names args ~f:(fun a b c -> (a,b,c)))
   *             ~f:(fun acc (pname, name, typ) ->
   *               [%expr GT.chain_compare [%e acc] (fun () ->
   *                 [%e
   *                   self#app_transformation_expr
   *                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
   *                     (Exp.ident ~loc pname)
   *                     (Exp.ident ~loc name)
   *                 ])]
   *             )
   *
   *         in
   *         case ~lhs ~guard:None ~rhs
   *       in
   *
   *       let other_case =
   *         let lhs = Pat.var ~loc "other" in
   *         let rhs = [%expr false ] in
   *         case ~lhs ~guard:None ~rhs
   *       in
   *
   *       Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
   *       Exp.match_ ~loc [%expr inh]
   *         [ main_case
   *         ; other_case
   *         ]
   *     ]]
   * ]
   *
   *
   * method app_transformation_expr trf inh subj =
   *   let loc = trf.pexp_loc in
   *   [%expr [%e trf ] [%e inh] [%e subj]]
   *
   * method abstract_trf ~loc k =
   *   [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]]
   *
   * method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
   *     constr_name bindings einh k =
   *   (\* TODO: rewrite *\)
   *   let ctuple =
   *     match bindings with
   *     | [] -> None
   *     | _  ->
   *       Some (Exp.tuple ~loc @@
   *             List.map bindings
   *               ~f:(fun (name, typ) ->
   *                   [%expr
   *                     [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
   *                     [%e einh]
   *                     [%e Exp.ident ~loc name ]
   *                   ]
   *                 )
   *            )
   *   in
   *   k @@ Exp.variant ~loc constr_name ctuple
   *
   * method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
   *   let pat = Pat.record ~loc ~flag:Closed @@
   *     List.map labs ~f:(fun l ->
   *         (Located.lident ~loc:l.pld_name.loc l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
   *       )
   *   in
   *   let methname = sprintf "do_%s" tdecl.ptype_name.txt in
   *   [ Cf.method_concrete ~loc methname
   *       [%expr fun () -> fun [%p pat ] -> [%e
   *         Exp.constant (const_string "asdf")
   *       ]]
   *   ] *)

end

let g =  (new g :>
            (Plugin_intf.plugin_args ->
              (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
