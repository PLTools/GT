open Base
open Ppxlib
open Printf
open Ast_helper
open Ppxlib.Ast_builder.Default
open GtHelpers

(* Compare plugin where we pass another value of the same type as 'inh
 * and return GT.comparison as 'syn
*)

open Plugin

class ['self] g initial_args = object(self: 'self)
  inherit ['self] Plugin.generator initial_args as super

  method plugin_name = "compare"

  method default_inh = core_type_of_type_declaration
  method syn_of_param ~loc s = [%type: GT.comparison]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: GT.comparison]

  method inh_of_param tdecl name =
    let loc = tdecl.ptype_loc in
    Typ.var ~loc name

  method plugin_class_params tdecl =
    let loc = tdecl.ptype_loc in
    List.map ~f:fst tdecl.ptype_params @ [self#extra_param_stub ~loc]

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    rhs_args @ [self#extra_param_stub ~loc]

  method! make_typ_of_self_trf ~loc tdecl =
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_typ_of_self_trf ~loc tdecl] ]

  method make_typ_of_class_argument ~loc tdecl name =
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_typ_of_class_argument ~loc tdecl name] ]

  method! make_RHS_typ_of_transformation ?subj_t ?syn_t tdecl =
    let loc = tdecl.ptype_loc in
    let subj_t = Option.value subj_t
        ~default:(using_type ~typename:tdecl.ptype_name.txt tdecl) in
    let syn_t  = Option.value syn_t  ~default:(self#default_syn tdecl) in
    [%type: [%t self#default_inh tdecl] ->
      [%t super#make_RHS_typ_of_transformation ~subj_t ~syn_t tdecl] ]

  method wrap_tr_function_str ~loc _tdelcl  make_gcata_of_class =
    let body = make_gcata_of_class [%expr self] in
    [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj]

  method chain_exprs ~loc e1 e2 =
    [%expr GT.chain_compare [%e e1] (fun () -> [%e e2]) ]
  method chain_init ~loc =
    [%expr GT.EQ ]

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
    let is_poly,cname =
      match constr_info with
      | `Normal s -> false,  s
      | `Poly   s -> true,   s
    in
    let methname = sprintf "c_%s" cname in
    let names     = List.map args ~f:(fun _ -> gen_symbol ()) in

    k @@ [
    Cf.method_concrete ~loc methname
      [%expr fun inh -> [%e
        let main_case =
          let pat_names = List.map args ~f:(fun _ -> gen_symbol ()) in
          let lhs =
            let arg_pats =
              match pat_names with
              | []  -> None
              | [s] -> Some (Pat.var ~loc s)
              | __  ->
                  Some (Pat.tuple ~loc @@ List.map pat_names ~f:(Pat.var ~loc))
            in
            if is_poly
            then Pat.variant   ~loc  cname         arg_pats
            else Pat.construct ~loc (lident cname) arg_pats
          in
          let rhs =
            List.fold_left  ~init:(self#chain_init ~loc)
              (List.map3_exn pat_names names args ~f:(fun a b c -> (a,b,c)))
              ~f:(fun acc (pname, name, typ) ->
                self#chain_exprs ~loc
                  acc
                  (self#app_transformation_expr
                     (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                     (Exp.ident ~loc pname)
                     (Exp.ident ~loc name)
                  )
              )
          in
          case ~lhs ~guard:None ~rhs
        in

        let other_case =
          let lhs = Pat.var ~loc "other" in
          let rhs =
            [%expr GT.compare_vari other
                [%e
                  (if is_poly then Exp.variant ~loc cname
                  else Exp.construct ~loc (lident cname)) @@
                  Exp.maybe_tuple ~loc @@
                  List.map args ~f:(fun _ -> [%expr Obj.magic ()])
                  (* It's annoying to use magic here but need to do this first:
                     https://caml.inria.fr/mantis/print_bug_page.php?bug_id=4751
                  *)
                ]]
          in
          case ~lhs ~guard:None ~rhs
        in

        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        Exp.match_ ~loc [%expr inh] [ main_case; other_case ]
      ]]
  ]

  method app_transformation_expr trf inh subj =
    let loc = trf.pexp_loc in
    [%expr [%e trf ] [%e inh] [%e subj]]

  method abstract_trf ~loc k =
    [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]]

  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    (* TODO: rewrite *)
    let ctuple =
      match bindings with
      | [] -> None
      | _  ->
        Some (Exp.tuple ~loc @@
              List.map bindings
                ~f:(fun (name, typ) ->
                    [%expr
                      [%e self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ ]
                      [%e einh]
                      [%e Exp.ident ~loc name ]
                    ]
                  )
             )
    in
    k @@ Exp.variant ~loc constr_name ctuple

  method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    assert Int.(List.length labs > 0);
    let pat = Pat.record ~loc ~flag:Closed @@
      List.map labs ~f:(fun {pld_name} ->
          (Located.lident ~loc:pld_name.loc pld_name.txt, Pat.var ~loc pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname
        (* TODO: maybe use abstract_transformation_expr here *)
        [%expr fun inh -> fun [%p pat ] -> [%e
          let wrap lab =
            self#app_transformation_expr
              (self#do_typ_gen ~loc ~is_self_rec ~mutal_names lab.pld_type)
              (Exp.field ~loc [%expr inh] (Located.lident ~loc lab.pld_name.txt))
              (Exp.ident ~loc lab.pld_name.txt )
          in
          let init = self#chain_init ~loc in
          List.fold_left ~init labs
            ~f:(fun acc lab -> self#chain_exprs ~loc acc (wrap lab))
        ]]
    ]

end

let g = new g
