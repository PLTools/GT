open Base
open HelpersBase
open Ppxlib
open Printf

let trait_name = "foldl"
(* TODO: we want easily get foldr from foldl *)
module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers
module P = Plugin.Make(AstHelpers)

let plugin_name =  trait_name
let make_dest_param_names  ps =
  map_type_param_names ps ~f:(sprintf "%s_2")

let hack_params ?(loc=noloc) ps =
  let param_names = map_type_param_names ps ~f:id in
  let rez_names = make_dest_param_names ps in
  let name_migrations = List.zip_exn param_names rez_names in
  let assoc s =
    try List.Assoc.find_exn ~equal:String.equal name_migrations s
    with Caml.Not_found ->
      raise_errorf "can't find new typ for param `%s" s
  in
  let blownup_params =
    List.concat_map param_names
      ~f:(fun s1 ->
           [Typ.var ~loc s1; Typ.var ~loc @@ assoc s1 ]
         )
  in
  (param_names, rez_names, assoc, blownup_params)

open Plugin

class g initial_args = object(self: 'self)
  inherit P.generator initial_args as super

  method plugin_name = "foldl"

  method default_inh ~loc tdecl = self#default_syn ~loc tdecl
  method default_syn ~loc tdecl = self#syn_of_param ~loc "dummy"

  method syn_of_param ~loc s = Typ.var ~loc "syn"
  method inh_of_param tdecl _ =
    self#syn_of_param ~loc:(loc_from_caml tdecl.ptype_loc) "dummy"

  method plugin_class_params tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    List.map tdecl.ptype_params ~f:(fun (t,_) -> typ_arg_of_core_type t) @
    [ named_type_arg ~loc "syn"
    ; named_type_arg ~loc Plugin.extra_param_name
    ]

    (* let loc = tdecl.ptype_loc in
     * (\* There we should have all parameters on the LHS of definition and 'syn one *\)
     * List.map ~f:fst tdecl.ptype_params @
     * [self#default_syn tdecl; self#extra_param_stub ~loc] *)

  method prepare_inherit_typ_params_for_alias ~loc tdecl rhs_args =
    List.map rhs_args ~f:Typ.from_caml @
    [ self#default_syn ~loc tdecl
    ; Typ.var ~loc Plugin.extra_param_name ]

    (* rhs_args @ [ self#default_syn tdecl ] @ [self#extra_param_stub ~loc] *)

  method! make_typ_of_self_trf ~loc tdecl =
    Typ.arrow ~loc (self#default_inh ~loc tdecl) (super#make_typ_of_self_trf ~loc tdecl)

  method make_typ_of_class_argument ~loc tdecl name k =
    super#make_typ_of_class_argument ~loc tdecl name (fun t ->
        k @@ Typ.arrow ~loc (self#default_inh ~loc tdecl) t )

  method! make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t  ~default:(self#default_syn ~loc tdecl) in
    Typ.arrow ~loc (self#default_inh ~loc tdecl)
      (super#make_RHS_typ_of_transformation ~loc ~subj_t ~syn_t tdecl)

  (* method wrap_tr_function_typ (typ: core_type) =
   *   let loc = typ.ptyp_loc in
   *   typ
   *   [%type: [%t self#syn_of_param] -> [%t self#default_inh] -> [%t typ] ] *)

  (* the same for foldl and eq, compare plugins. Should be moved out *)
  method wrap_tr_function_str ~loc _tdelcl  make_gcata_of_class =
    let body = make_gcata_of_class (Exp.ident ~loc "self") in
    Exp.fun_list ~loc [ Pat.sprintf ~loc "the_init"; Pat.sprintf ~loc "subj"] @@
    Exp.app_list ~loc
      (Exp.of_longident ~loc (Ldot (Lident "GT", "fix0")))
      [ Exp.fun_ ~loc (Pat.sprintf ~loc "self") body
      ; Exp.sprintf ~loc "the_init"
      ; Exp.sprintf ~loc "subj"
      ]


  method generate_for_polyvar_tag ~loc ~is_self_rec ~mutal_names
      constr_name bindings einh k =
    k @@ Exp.variant ~loc constr_name @@
    List.map bindings
      ~f:(fun (name, typ) ->
                    Exp.app_list ~loc
                      (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
                      [ einh
                      ; Exp.ident ~loc name
                      ]
      )

  method on_tuple_constr ~loc ~is_self_rec ~mutal_names tdecl constr_info args k =
    let names = List.map args ~f:(fun _ -> gen_symbol ()) in
    let methname = sprintf "c_%s" (match constr_info with `Normal s -> s | `Poly s -> s) in
    k [
      (* TODO: inh syn stuff *)
    Cf.method_concrete ~loc methname @@
    Exp.fun_ ~loc (Pat.sprintf ~loc "inh") @@
    Exp.fun_list ~loc (List.map names ~f:(Pat.sprintf ~loc "%s")) @@
    List.fold_left ~f:(fun acc (name,typ) ->
        Exp.app_list ~loc
          (self#do_typ_gen ~loc ~is_self_rec ~mutal_names typ)
          [ acc; Exp.sprintf ~loc "%s" name]
        )
        ~init:(Exp.ident ~loc "inh")
        (List.zip_exn names args)

  ]

  method on_record_declaration ~loc ~is_self_rec ~mutal_names tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_list ~loc
        [ Pat.sprintf ~loc "inh"; pat]
        (Exp.failwith_ ~loc "not implemented")

    ]

end

let g = (new g :> (Plugin_intf.plugin_args ->
                   (loc, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g) )

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
