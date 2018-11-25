(** A few base classes for plugins with virtual methods to be implemented.

    See {!Plugin_intf} for complete description of all valuable methods
  *)

open Base
open Ppxlib
open Printf
open Asttypes
open HelpersBase

let extra_param_name = Naming.extra_param_name

module Make(AstHelpers : GTHELPERS_sig.S) = struct

open AstHelpers
module Intf = Plugin_intf.Make(AstHelpers)

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
  let k cs = Exp.match_ ~loc what cs in
  let rs =
    List.map rows ~f:(function
        | Rtag (lab, _, _, args) ->
          let args = match args with
            | [t] -> unfold_tuple t
            | [] -> []
            | _ -> failwith "we don't support conjunction types"
          in
          let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
          let lhs = Pat.variant ~loc lab.txt @@ List.map ~f:(Pat.var ~loc) names in
          case ~lhs ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;_},ts) ->
            let newname = "subj" in
            let lhs = Pat.alias ~loc (Pat.type_ ~loc txt) newname
            in
            case ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let ls = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = Pat.alias ~loc (Pat.type_ ~loc (Lident lab) ) newname
        in
        case ~lhs ~rhs:(onlabel lab newname)
      )
  in
  k @@ rs@ls


(** Base class for all plugins. Implements {!Plugin_intf.typ_g} interface

    Is subclassed by {!with_inherit_arg} and {!no_inherite_arg}. Use them for convenience.
*)
class virtual generator initial_args = object(self: 'self)
  inherit Intf.g

  method plugin_name = self#trait_name
  (* parse arguments like { _1=<expr>; ...; _N=<expr>; ...} *)
  val reinterpreted_args =
    let check_name s =
      try Caml.Scanf.sscanf s "_%d" (fun n -> Some n)
      with Caml.Scanf.Scan_failure _ ->
        (* Stdio.printf "can't parse it\n%!"; *)
        None
    in
    let ans =
      List.fold_left initial_args ~init:[]
        ~f:(fun acc (lident,expr) ->
            match lident with
            | Lident s -> Option.value_map (check_name s) ~default:acc
                            ~f:(fun n -> (n,expr) :: acc)
            | _ -> acc
          )
    in
    (* printf "Total args found for plugin : %d\n%!"  (List.length ans); *)
    ans

  method show_args =
    (* Stdio.printf "showing %d args\n%!" (List.length reinterpreted_args); *)
    List.iter reinterpreted_args ~f:(fun (k,e) ->
        Format.printf "%d -> %a\n%!" k Pprintast.expression e
      )

  method extra_class_sig_members _ = []
  method extra_class_str_members _ = []
  method cur_name tdecl = tdecl.ptype_name.txt

  (* preparing class of transformation for [tdecl] *)
  method make_class ~loc ~is_rec mutal_decls tdecl =
    let cur_name = self#cur_name tdecl in
    let k fields =
      let inh_params =
        prepare_param_triples ~loc
          ~inh:(fun ~loc -> self#inh_of_param tdecl)
          ~syn:self#syn_of_param
          ~default_syn:(self#default_syn ~loc tdecl)
          ~default_inh:(self#default_inh ~loc tdecl)
          ~extra:(Typ.var ~loc @@
                  sprintf "%s_%s" Naming.extra_param_name tdecl.ptype_name.txt)
          (map_type_param_names tdecl.ptype_params ~f:id)
      in
      self#wrap_class_definition ~loc mutal_decls tdecl ~inh_params
        ((self#extra_class_str_members tdecl) @ fields)
    in

    let is_self_rec t =
      is_rec &&
      match t.ptyp_desc with
      | Ptyp_var _ -> false
      | Ptyp_constr ({txt=Lident s}, params)
        when String.equal s cur_name &&
             List.length params = List.length tdecl.ptype_params &&
             List.for_all2_exn params tdecl.ptype_params
               ~f:(fun a (b,_) -> 0=compare_core_type a b)
        -> is_rec
      | _ -> false
    in
    self#got_typedecl ~loc tdecl ~is_self_rec ~mutal_decls k

  method prepare_fa_args ~loc tdecl =
    map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")

  method wrap_class_definition ~loc ~inh_params mutal_decls tdecl fields =
    let cur_name = self#cur_name tdecl in
    (* inherit class_t and prepare to put other members *)

    Str.class_single ~loc
      ~params:(self#plugin_class_params tdecl)
      ~name:(self#make_class_name ~is_mutal:(not (List.is_empty mutal_decls)) tdecl)
      ~virt:false
      ~wrap:(fun body ->
          (* constructor arguments are *)
          let names =
            (match mutal_decls with
            | [] -> []
            | _ -> [Pat.sprintf ~loc "%s" @@ Naming.mut_arg_composite])
            (* List.map mutal_decls ~f:(fun td ->
             *     Pat.sprintf ~loc "%s" @@
             *     Naming.mut_arg_name ~plugin:self#plugin_name td.ptype_name.txt
             *   ) *)
            @ (self#prepare_fa_args ~loc tdecl)
            @ [Pat.var ~loc Naming.self_arg_name]
          in
          Cl.fun_list ~loc names body
      )
      @@
      [ let parent_name = Naming.class_name_for_typ cur_name in
        Cf.inherit_ ~loc (Cl.constr ~loc (Lident parent_name) inh_params)
      ] @ fields


  method virtual make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a

  (* next method should be synchronized with prepare_fa_args *)
  (* method prepare_fa_arg_types ~loc tdecl =
   *   let names = map_type_param_names tdecl.ptype_params ~f:id  in
   *   List.map names
   *     ~f:(fun name ->
   *         self#make_typ_of_class_argument
   *           ~loc
   *           tdecl
   *           name
   *           (fun x -> x)
   *       ) *)

  (* signature for a plugin class *)
  method make_class_sig ~loc tdecl ~is_rec mutal_decls =
    let k fields =
      [ Sig.class_ ~loc
          ~params:(self#plugin_class_params tdecl)
          ~name:(self#make_class_name ~is_mutal:false tdecl)
          ~virt:false
          ~wrap:(fun sign ->
              let for_self = self#make_typ_of_self_trf ~loc tdecl in
              let funcs_for_args =
                let names = map_type_param_names tdecl.ptype_params ~f:id in
                List.fold_left names
                  ~init:id
                  ~f:(fun acc name ->
                      self#make_typ_of_class_argument ~loc tdecl (Cty.arrow ~loc) name
                        (fun f arg -> acc @@ f arg)
                    )
                  (Cty.arrow ~loc for_self sign)
              in

              List.fold_right
                (List.filter mutal_decls
                   ~f:(fun t -> String.(<>) t.ptype_name.txt tdecl.ptype_name.txt))
                ~init:funcs_for_args
                ~f:(fun mut_decl acc ->
                  self#make_typ_of_mutal_trf ~loc mut_decl
                    (fun t -> Cty.arrow ~loc t acc)
                )
            )
          ((self#extra_class_sig_members tdecl) @ fields)
      ]
    in
    visit_typedecl ~loc tdecl
      ~onabstract:(fun () -> k [])
      ~onrecord:(fun _fields ->
          k [ Ctf.method_ ~loc (Naming.meth_name_for_record tdecl) ~virt:false @@
              Typ.chain_arrow ~loc @@
              let open Typ in
              [ self#default_inh ~loc tdecl
              ; use_tdecl tdecl ] @
              [ self#default_syn ~loc tdecl ]
            ]
        )
      ~onmanifest:(fun typ ->
        let rec helper typ =
          match typ.ptyp_desc with
          | Ptyp_var name -> (* antiphantom types *)
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]

          | Ptyp_alias (t, aname) ->
            let loc = t.ptyp_loc in
            map_core_type t ~onvar:(fun as_ ->
              let open Ppxlib.Ast_builder.Default in
              if String.equal as_ aname
              then Option.some @@
                ptyp_constr ~loc (Located.lident ~loc tdecl.ptype_name.txt) @@
                List.map tdecl.ptype_params ~f:(fun (t,_) -> t)
              else Option.some @@ ptyp_var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
            (* there for type 'a list = ('a,'a list) alist
             * we inherit plugin class for base type, for example (gmap):
             *  inherit ('a,'a2,'a list,'a2 list) gmap_alist
             **)
            k [Ctf.inherit_ ~loc @@ Cty.constr ~loc
                 (map_longident cid.txt
                    ~f:(Naming.trait_class_name_for_typ ~trait:self#trait_name))
                 (self#final_typ_params_for_alias ~loc tdecl params)
              ]
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc:typ.ptyp_loc ts
          | Ptyp_variant (rows,_,_) ->
              let rr = List.map rows ~f:(function
              | Rinherit typ ->
                  with_constr_typ typ
                    ~ok:(fun cid params ->
                        Ctf.inherit_ ~loc @@
                        Cty.constr ~loc
                          (map_longident  cid.txt
                             ~f:(Naming.trait_class_name_for_typ ~trait:self#plugin_name))
                          (self# final_typ_params_for_alias ~loc
                             tdecl params)
                     )
                     ~fail:(fun () -> assert false)
              | Rtag (lab,_,_, typs) -> begin
                  Ctf.method_ ~loc (sprintf "c_%s" lab.txt) ~virt:false @@
                  match typs with
                  | [] -> Typ.(chain_arrow ~loc
                                [ self#default_inh ~loc tdecl
                                ; use_tdecl tdecl
                                ; self#default_syn ~loc tdecl]
                              )
                  | [t] ->
                      Typ.(chain_arrow ~loc @@
                             [ self#default_inh ~loc tdecl
                             ; use_tdecl tdecl ] @
                             (List.map ~f:Typ.from_caml @@ unfold_tuple t) @
                             [self#default_syn ~loc tdecl]
                          )
                  | typs ->
                      Typ.(chain_arrow ~loc @@
                             [ self#default_inh ~loc tdecl
                             ; use_tdecl tdecl ] @
                             (List.map ~f:Typ.from_caml typs) @
                             [self#default_syn ~loc tdecl]
                          )
                end
              )
              in
              k @@ rr
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds ->
        k @@ List.map cds ~f:(fun cd ->
              let typs = match cd.pcd_args with
                | Pcstr_record ls -> List.map ls ~f:(fun x -> x.pld_type)
                | Pcstr_tuple ts -> ts
              in
              let new_ts =
                let open Typ in
                [ self#default_inh ~loc tdecl
                ; use_tdecl tdecl ] @
                (List.map typs ~f:Typ.from_caml) @
                [ self#default_syn ~loc tdecl ]
              in

              Ctf.method_ ~loc ~virt:false (Naming.meth_of_constr cd.pcd_name.txt) @@
              Typ.chain_arrow ~loc new_ts
          )
    )

  method make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams =
    let args =
      List.mapi cparams ~f:(fun i t ->
          (* Stdio.printf "checking for arg with index (%d+1)\n%!" i; *)
          try List.Assoc.find_exn reinterpreted_args ~equal:Int.equal (i+1)
            |> Exp.from_caml
          with Caml.Not_found -> do_typ ~loc t
        )
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    args (* @ [ Exp.sprintf ~loc "%s" Naming.self_arg_name] *)


  (* When we got declaration of type alias via type application *)
  method got_constr ~loc ~is_self_rec tdecl mutal_decls do_typ cid cparams k =
    (* It seems that we can't filter mutal decls because we need to preserve an order *)
    let mutal_names = List.map mutal_decls ~f:(fun t -> t.ptype_name.txt) in
    let ans args : Cf.t list =
      [ let typ_params = self#final_typ_params_for_alias ~loc tdecl cparams
        in
        let args,fixident =
          match cid.txt with
          | Lident s when List.mem mutal_names s ~equal:String.equal
            ->
            (* Only Lident because we ignore types with same name but from another module *)
            let extra_args =
              match mutal_names with
              | [] -> []
              | _ -> [Exp.ident ~loc @@ Naming.mut_arg_composite]
              (* List.map mutal_names
               *   ~f:(fun name -> Exp.ident ~loc @@
               *        Naming.mut_arg_name ~plugin:self#plugin_name name) *)
            in
            (extra_args @ args,
             (fun s -> s ^ "_stub")
            )
          | _ -> (args, id)
        in
        Cf.inherit_ ~loc @@ Cl.apply ~loc
          (Cl.constr ~loc
             (map_longident cid.txt
                ~f:(fun s -> fixident @@
                    Naming.trait_class_name_for_typ ~trait:self#plugin_name s
                  ))
             typ_params)
          args
      ]
    in

    let class_args =
      (* TODO: maybe we should hardcode fself here and skip it in plugins *)
      (self#make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams)
      @ [Exp.ident ~loc Naming.self_arg_name]
    in
    k @@ ans class_args


  method got_polyvar ~loc ~is_self_rec ~mutal_decls tdecl do_typ rows k =
    List.concat_map rows ~f:(function
    | Rinherit typ ->
        with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
                (* Hypothesis: it's almost an type alias *)
                self#got_constr ~loc ~is_self_rec tdecl mutal_decls do_typ cid params k
            )
    (* TODO: Do something with copy paste. *)
    (* tag by default have 1 argument which is a tuple instead of many arguments *)
    | Rtag (constr_name,_,_, []) ->
      k [
        let inhname = gen_symbol ~prefix:"inh_" () in
        Cf.method_concrete ~loc (Naming.meth_name_for_constructor constr_name.txt) @@
        Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
        Exp.fun_ ~loc (Pat.any ~loc) @@
        self#on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe:(Exp.ident ~loc inhname)
          (`Poly constr_name.txt) []
      ]
    | Rtag (constr_name,_,_, [arg]) ->
      k [
        let inhname = gen_symbol ~prefix:"inh_" () in
        let bindings = List.map (unfold_tuple arg) ~f:(fun ts -> gen_symbol (), ts) in
        Cf.method_concrete ~loc (Naming.meth_name_for_constructor constr_name.txt) @@
        Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
        Exp.fun_ ~loc (Pat.any ~loc) @@
        self#on_tuple_constr ~loc ~is_self_rec ~mutal_decls ~inhe:(Exp.ident ~loc inhname)
          (`Poly constr_name.txt) bindings
      ]
    | Rtag (constr_name,_,_,args) ->
      (* Hypothesis: it's almost the same as constructor with a tuple of types  *)
      failwith "conjunction types are not supported but"
    )

  method got_typedecl ~loc ~is_self_rec ~mutal_decls tdecl (k: Cf.t list -> _) =
    k @@
    visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let rec helper typ  =
          match typ.ptyp_desc with
          | Ptyp_var name -> (* antiphantom types *)
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]
          | Ptyp_alias (t, aname) ->
            let open Ppxlib.Ast_builder.Default in
            let loc = tdecl.ptype_loc in
            map_core_type t ~onvar:(fun as_ ->
              if String.equal as_ aname
              then Option.some @@
                ptyp_constr ~loc:t.ptyp_loc
                  (Located.lident ~loc tdecl.ptype_name.txt)
                  (List.map tdecl.ptype_params ~f:fst)
              else Option.some @@ ptyp_var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
              self#got_constr ~loc ~is_self_rec tdecl mutal_decls
                (self#do_typ_gen ~mutal_decls ~is_self_rec)
                cid params (fun x -> x)

          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc:typ.ptyp_loc ts
          | Ptyp_variant (rows,_,_) ->
            self#got_polyvar ~loc tdecl (self#do_typ_gen ~mutal_decls ~is_self_rec)
              ~is_self_rec ~mutal_decls rows (fun x -> x)
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds -> self#on_variant ~loc ~mutal_decls ~is_self_rec tdecl cds id)
    ~onrecord:(self#on_record_declaration ~loc ~is_self_rec ~mutal_decls tdecl)
    ~onopen:(fun () -> [])

  method virtual on_record_declaration: loc:loc ->
    is_self_rec:(core_type -> bool) ->
    mutal_decls:(type_declaration list) ->
    type_declaration ->
    label_declaration list ->
    Cf.t list

  method make_typ_of_mutal_trf ~loc mutal_tdecl (k: Typ.t -> _)  =
    let subj_t = Typ.use_tdecl mutal_tdecl in
    k Typ.(arrow ~loc subj_t (self#default_syn ~loc mutal_tdecl))

    (* k @@ Typ.from_caml [%type: ([%t subj_t] -> [%t self#default_syn ~loc mutal_tdecl]) ] *)


  (* val name : <typeof fa> -> ... -> <typeof fz> ->
                     <this type we are generating here>
  *)

  (* method make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
   *   let subj_t = Option.value subj_t
   *       ~default:(Typ.use_tdecl tdecl) in
   *   let syn_t  = Option.value syn_t ~default:(self#default_syn ~loc tdecl) in
   *   Typ.arrow ~loc subj_t syn_t *)

  (* method chain_inh_syn ~loc ~inh_t ~syn_t subj_t =
   *   [%type: [%t inh_t] -> [%t subj_t] -> [%t syn_t] ] *)

  method wrap_tr_function_typ (typ: core_type) = typ

  method make_trans_function_typ ~loc tdecl =
    let type_ = self#make_RHS_typ_of_transformation ~loc tdecl in
    let names = map_type_param_names tdecl.ptype_params ~f:id in
    List.fold_left names
      ~init:id
      ~f:(fun acc name ->
          self#make_typ_of_class_argument ~loc tdecl (Typ.arrow ~loc) name
            (fun f arg -> acc @@ f arg)
        )
      type_

  method make_trans_function_name tdecl =
    sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt

  method make_trans_functions_sig: loc:loc ->
    is_rec:bool -> string list -> type_declaration list -> Sig.t list
    = fun ~loc ~is_rec mutal_names tdecls ->

      List.map tdecls ~f:(fun tdecl ->
          let type_ = self#make_trans_function_typ ~loc tdecl in
          Sig.value ~loc
            ~name:(self#make_trans_function_name tdecl)
            type_
        )

  method make_class_name ?(is_mutal=false) tdecl =
    sprintf "%s%s"
      (Naming.trait_class_name_for_typ ~trait:self#plugin_name
         tdecl.ptype_name.txt)
      (if is_mutal then "_stub" else "")

  method wrap_tr_function_str ~loc tdecl make_new_obj =
    let body = make_new_obj in

    Exp.fun_ ~loc (Pat.sprintf ~loc "subj") @@
    Exp.app_list ~loc
      (Exp.of_longident ~loc (Ldot (Lident "GT", "transform_gc")) )
      [ Exp.sprintf ~loc "gcata_%s" tdecl.ptype_name.txt (* TODO: name *)
      ; make_new_obj
      (* ; Exp.fun_ ~loc (Pat.sprintf ~loc "self") (Exp.app ~loc body @@ Exp.unit ~loc ) *)
      ; Exp.sprintf ~loc "subj"
      ]
    (* [%expr fun subj -> GT.fix0 (fun self ->
     *     [%e body] ()) subj
     * ] *)

  method apply_fas_in_new_object ~loc tdecl =
    (* very similar to self#make_inherit_args_for_alias but the latter
     * applies `fself` by default. Need to refactor and remove this function *)
    map_type_param_names tdecl.ptype_params ~f:(Exp.sprintf ~loc "f%s")

  (* let <plugin-name> fa ... fz = <this body> *)
  method make_trans_function_body ~loc ?(rec_typenames=[]) class_name tdecl =
    self#wrap_tr_function_str ~loc tdecl
      (  Exp.app_list ~loc (Exp.new_ ~loc @@ Lident class_name) @@
         (
          List.map rec_typenames ~f:(Exp.sprintf ~loc "%s_%s" self#plugin_name)
          @ (self#apply_fas_in_new_object ~loc tdecl)
         )
      )

  method make_trans_functions: loc:loc ->
    is_rec:bool -> string list -> type_declaration list -> Str.t
    = fun ~loc ~is_rec mutal_names tdecls ->
      (* we will generate mutally recursive showers here *)
      let on_tdecl tdecl =
        let cur_name = tdecl.ptype_name.txt in
        let others =
          List.filter mutal_names ~f:(String.(<>) cur_name)
        in
        value_binding ~loc
          ~pat:(Pat.sprintf ~loc "%s" @@ self#make_trans_function_name tdecl)
          ~expr:(
            let class_name = self#make_class_name
                ~is_mutal:(not (List.is_empty mutal_names))
                tdecl
            in
            Exp.fun_list ~loc
              (self#prepare_fa_args ~loc tdecl)
              (self#make_trans_function_body ~loc ~rec_typenames:others
                 class_name tdecl)
          )
      in
      Str.values ~loc @@ List.map tdecls ~f:on_tdecl


  method do_single_sig ~loc ~is_rec tdecl =
    List.concat
    [ self#make_class_sig ~loc ~is_rec tdecl []
    ; self#make_trans_functions_sig ~loc ~is_rec [] [tdecl]
    ]

  method do_single ~loc ~is_rec tdecl =
    [ self#make_class ~loc ~is_rec [] tdecl
    ; self#make_trans_functions ~loc ~is_rec [] [tdecl]
    ]

  method final_typ_params_for_alias ~loc tdecl rhs =
    self#prepare_inherit_typ_params_for_alias ~loc tdecl rhs @
    [ Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt ]

  method do_mutals ~loc ~is_rec tdecls : Str.t list =
    (* for mutal recursion we need to generate two classes, one transformation
       function many structures, fixpoint, etc.
    *)
    let mut_names = List.map tdecls ~f:(fun td -> td.ptype_name.txt) in
    List.map tdecls ~f:(self#make_trf_field_tdecl ~loc) @
    [ let name = Naming.prereq_name self#plugin_name
          (List.hd_exn tdecls).ptype_name.txt
      in
      Str.tdecl_record ~loc ~name ~params:[] @@
      List.map tdecls ~f:(fun tdecl ->
          let lab_name = Naming.typ1_for_class_arg ~plugin:self#plugin_name
              tdecl.ptype_name.txt  in
          lab_decl ~loc (Naming.trf_function self#plugin_name tdecl.ptype_name.txt)
            false
            (Typ.ident ~loc lab_name)
        )
    ] @
    List.map tdecls ~f:(self#make_class ~loc ~is_rec:true tdecls) @
    (self#make_universal_types  ~loc ~mut_names tdecls) @
    [self#make_mutal_fix ~loc tdecls] @
    (self#apply_mutal_fix ~loc tdecls) @
    (self#make_shortend_class ~loc tdecls)

  method simple_trf_funcs ~loc tdecl : Typ.t -> Typ.t =
    let names = map_type_param_names tdecl.ptype_params ~f:id in
    List.fold_left names
      ~init:id
      ~f:(fun acc name ->
          self#make_typ_of_class_argument ~loc tdecl
            (Typ.arrow ~loc) name
            (fun f arg -> acc @@ f arg)
        )

  method make_mutal_fix ~loc tdecls =
    Str.single_value ~loc (Pat.sprintf ~loc "%s" @@
                           Naming.make_fix_name ~plugin:self#plugin_name tdecls) @@
    Exp.fun_list_l ~loc (List.map tdecls ~f:(fun tdecl ->
        ( tdecl.ptype_name.txt ^ "0"
        , Exp.record1 ~loc
            (lident @@ Naming.mut_oclass_field ~plugin:self#plugin_name
               tdecl.ptype_name.txt)
            (Exp.new_ ~loc
               (lident @@ Naming.stub_class_name ~plugin:self#plugin_name tdecl)
            )
        )
      )) @@
    Exp.fun_ ~loc (Pat.unit ~loc) @@
    Exp.let_ ~loc ~rec_:true
      (List.concat_map tdecls ~f:(fun tdecl ->
           let typ_name = tdecl.ptype_name.txt in
           let t0field = Naming.mut_oclass_field ~plugin:self#plugin_name typ_name in
           let trf_field = Naming.trf_field ~plugin:self#plugin_name typ_name in
           let trf_r_ident = sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt in
           let othernames = List.map tdecls ~f:(fun tdecl -> tdecl.ptype_name.txt) in
           let eplugin =
             let open Exp in
             fun_list ~loc
               (map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")) @@
             self#abstract_trf ~loc @@ fun einh esubj ->
             self#app_transformation_expr ~loc
               (self#app_gcata ~loc @@
                let fas = map_type_param_names tdecl.ptype_params
                    ~f:(sprintf ~loc "f%s")
                in
                app ~loc
                  (ident ~loc @@ Naming.gcata_name_for_typ tdecl.ptype_name.txt)
                  (app_list ~loc
                     (field ~loc (sprintf ~loc "%s0" typ_name) (lident t0field))
                   @@
                   (* List.map othernames ~f:(sprintf ~loc "%s_%s" self#plugin_name) @ *)
                   [ record ~loc @@ List.map othernames ~f:(fun s ->
                         (lident @@ Naming.trf_function self#plugin_name s,
                          sprintf ~loc "%s_%s" self#plugin_name s
                         )
                       )
                   ] @
                   fas @
                   [ app_list ~loc
                       (field ~loc (ident ~loc trf_r_ident) (lident trf_field))
                       fas
                   ]
                  )
               )
               einh
               esubj
           in

           [ Pat.sprintf ~loc "%s" trf_r_ident,
             Exp.record1 ~loc
               (lident @@ Naming.trf_field ~plugin:self#plugin_name tdecl.ptype_name.txt) @@
             eplugin
           ]
         ))
      (Exp.record ~loc @@
       List.map tdecls ~f:(fun tdecl ->
           (lident @@ Naming.trf_function self#plugin_name tdecl.ptype_name.txt,
            Exp.ident ~loc @@ Naming.trf_function self#plugin_name tdecl.ptype_name.txt
           )
         )
      )

  method apply_mutal_fix ~loc tdecls =
    [ Str.single_value ~loc
        (Pat.alias ~loc
          (Pat.record ~loc @@ List.map tdecls ~f:(fun tdecl ->
           (lident @@ Naming.trf_function self#plugin_name tdecl.ptype_name.txt,
            Pat.sprintf ~loc "%s" @@ Naming.fix_result tdecl
           )
             ))
          Naming.all_trfs_together
        )
        (Exp.app ~loc (Exp.ident ~loc @@
                       Naming.make_fix_name ~plugin:self#plugin_name tdecls) @@
         Exp.unit ~loc
        )
    ] @ List.map tdecls ~f:(fun tdecl ->
        Str.single_value ~loc
          (Pat.sprintf ~loc "%s" @@ Naming.trf_function self#plugin_name tdecl.ptype_name.txt) @@
        Exp.fun_list ~loc
          (map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")) @@
        Exp.fun_ ~loc (Pat.sprintf ~loc "subj") @@
        Exp.app ~loc
          (Exp.app_list ~loc
             (Exp.field ~loc (Exp.sprintf ~loc "%s" @@ Naming.fix_result tdecl)
                (lident @@ Naming.trf_field
                   ~plugin:self#plugin_name tdecl.ptype_name.txt)) @@
             (map_type_param_names tdecl.ptype_params ~f:(Exp.sprintf ~loc "f%s"))
          )
          (Exp.sprintf ~loc "subj")
      )

  method make_trf_field_tdecl ~loc tdecl =
    let name = tdecl.ptype_name.txt in
    Str.tdecl_record ~loc ~params:[]
      ~name:(Naming.typ1_for_class_arg ~plugin:self#plugin_name name)
      [lab_decl ~loc (Naming.trf_field name ~plugin:self#plugin_name)
         false
         (let t = self#make_trans_function_typ ~loc tdecl in
          match typ_vars_of_typ t with
          | [] -> t
          | xs -> Typ.poly ~loc xs t
         )
      ]
  method make_universal_types ~loc ~mut_names tdecls =
    (* let obj_typs = List.fold_right tdecls
     *     ~init:[]
     *     ~f:(fun tdecl acc ->
     *         let t =
     *           Typ.arrow ~loc (Typ.ident ~loc "unit") @@
     *           self#simple_trf_funcs ~loc tdecl @@
     *           Typ.(constr ~loc
     *                (lident @@ self#make_class_name ~is_mutal:true tdecl)
     *                (self#plugin_class_params tdecl
     *                 |> List.map ~f:(Typ.of_type_arg ~loc))
     *               )
     *         in
     *         List.Assoc.add acc ~equal:String.equal tdecl.ptype_name.txt (tdecl, t)
     *   )
     * in *)
    let prereq_typ tl =
      Typ.arrow ~loc
        (Typ.ident ~loc @@
         Naming.prereq_name self#plugin_name (List.hd_exn tdecls).ptype_name.txt)
        tl
    in
    List.concat_map tdecls ~f:(fun tdecl ->
        let name = tdecl.ptype_name.txt in
        [ Str.tdecl_record ~loc ~params:[]
            ~name:(Naming.typ3_for_class_arg ~plugin_name:self#plugin_name name)
            [lab_decl ~loc (Naming.mut_oclass_field ~plugin:self#plugin_name name)
               false
               begin
                 let part1 =
                   self#simple_trf_funcs ~loc tdecl @@
                   Typ.arrow ~loc (self#make_typ_of_self_trf ~loc tdecl) @@
                   Typ.(constr ~loc
                            (lident @@ self#make_class_name ~is_mutal:true tdecl)
                            (self#plugin_class_params tdecl
                             |> List.map ~f:(Typ.of_type_arg ~loc))
                         )
                 in
                 (* let part1 =
                  *   Typ.arrow ~loc (self#make_typ_of_self_trf ~loc tdecl) part1
                  * in *)

                 (* let t = List.fold_right obj_typs
                  *     ~init:part1
                  *     ~f:(fun (key,(other_tdecl, otyp)) acc ->
                  *         (\* we need to try specialize [other_tdecl] using its
                  *            occurances in [tdecl] *\)
                  *         (\* TODO: rewrite this piece of code *\)
                  *         Typ.arrow ~loc
                  *           (Typ.sprintf ~loc "%s"  @@
                  *            Naming.typ1_for_class_arg ~plugin:self#plugin_name key)
                  *           acc
                  *       )
                  * in *)
                 let t = prereq_typ part1 in
                 match typ_vars_of_typ t with
                 | [] -> t
                 | xs -> Typ.poly ~loc xs t
               end
            ]
        ]
      ) (* @ [
     *   Str.tdecl_record ~loc ~params:[]
     *     ~name:(Naming.fix_result_record self#plugin_name tdecls) @@
     *   List.map tdecls ~f:(fun tdecl ->
     *       lab_decl ~loc (Naming.trf_function self#plugin_name tdecl)
     *         false
     *         Typ.(ident ~loc @@
     *              Naming.typ1_for_class_arg ~plugin:self#plugin_name tdecl.ptype_name.txt)
     *     )
     * ] *)

  (* method specialize tdecl typ (map: (string * Ppxlib.core_type) List.t) : Typ.t =
   *   let (_:type_declaration) = tdecl in
   *   Typ.map typ
   *     ~onvar:(fun s ->
   *         Option.map ~f:Typ.from_caml @@
   *         (List.Assoc.find ~equal:String.equal map s)
   *       ) *)

  (* shortened class only used for mutally recursive declarations *)
  method make_shortend_class ~loc tdecls =
    List.map tdecls ~f:(fun tdecl ->
        let typname = tdecl.ptype_name.txt in
        (* let mutal_decls = List.filter tdecls
         *     ~f:(fun td -> not (String.equal td.ptype_name.txt typname))
         * in *)
        let class_name =
          Naming.trait_class_name_for_typ ~trait:self#plugin_name typname
        in
        let stub_name = Naming.stub_class_name ~plugin:self#plugin_name tdecl in
        (* maybe it should be called proto *)
        let mut_funcs =
          [Exp.ident ~loc Naming.all_trfs_together]
        in

      let params = self#plugin_class_params tdecl in
      Str.class_single ~loc ~name:class_name
        ~wrap:(fun cl ->
            Cl.fun_ ~loc (Pat.sprintf ~loc "%s" Naming.self_arg_name) @@
            Cl.fun_list ~loc (self#prepare_fa_args ~loc tdecl) cl
          )
        ~params
        [ Cf.inherit_ ~loc @@ Cl.apply ~loc
            (Cl.constr ~loc (Lident stub_name) @@
             List.map ~f:(Typ.of_type_arg ~loc) params)
            (mut_funcs @
             [Exp.sprintf ~loc "%s" Naming.self_arg_name] @
             (self#apply_fas_in_new_object ~loc tdecl))
        ]
    )

  method virtual on_record_constr: loc:loc ->
    is_self_rec:(core_type -> bool) ->
    mutal_decls:type_declaration list ->
    inhe:Exp.t ->
    [ `Normal of string | `Poly of string ] ->
    (* pattern variable, label name, typ of label *)
    (string * string * core_type) list ->
    label_declaration list -> 
    Exp.t

  method virtual on_tuple_constr : loc:loc ->
    is_self_rec:(core_type -> bool) ->
    mutal_decls:type_declaration list ->
    inhe:Exp.t ->
    [ `Normal of string | `Poly of string ] ->
    (string * core_type) list ->
    Exp.t

  method on_variant ~loc tdecl ~mutal_decls ~is_self_rec cds k =
    k @@
    List.map cds ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_tuple ts ->
          let inhname = gen_symbol ~prefix:"inh_" () in
          let loc = loc_from_caml cd.pcd_loc in
          let bindings = List.map ts ~f:(fun ts -> gen_symbol (), ts) in
          Cf.method_concrete ~loc (Naming.meth_name_for_constructor cd.pcd_name.txt) @@
          Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
          Exp.fun_ ~loc (Pat.any ~loc) @@
          self#on_tuple_constr ~loc ~mutal_decls ~is_self_rec
            ~inhe:(Exp.ident ~loc inhname)
            (`Normal cd.pcd_name.txt) bindings
        | Pcstr_record ls ->
            let inhname = gen_symbol ~prefix:"inh_" () in
            let loc = loc_from_caml cd.pcd_loc in
            let bindings =
              List.map ls ~f:(fun l -> gen_symbol (), l.pld_name.txt, l.pld_type)
            in
            Cf.method_concrete ~loc (Naming.meth_name_for_constructor cd.pcd_name.txt) @@
            Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
            Exp.fun_ ~loc (Pat.any ~loc) @@
            self#on_record_constr ~loc ~mutal_decls ~is_self_rec
              ~inhe:(Exp.ident ~loc inhname)
              (`Normal cd.pcd_name.txt)
              bindings
              ls
      )

  method generate_for_variable ~loc varname =
    Exp.sprintf ~loc "f%s" varname

  method virtual app_transformation_expr: loc:loc ->
    Exp.t -> Exp.t -> Exp.t -> Exp.t

  method virtual abstract_trf: loc:loc -> (Exp.t -> Exp.t -> Exp.t) -> Exp.t

  (* method do_typext_str ~loc ({ptyext_path } as extension) =
   *   let clas =
   *     let is_self_rec _ = false in
   *     let cds = List.map extension.ptyext_constructors
   *         ~f:(fun ec ->
   *             match ec.pext_kind with
   *             | Pext_rebind _ -> failwith ""
   *             | Pext_decl (args, _) ->
   *               Ast_builder.Default.constructor_declaration
   *                 ~loc:extension.ptyext_path.loc ~res:None
   *                 ~name:(ec.pext_name) ~args
   *           )
   *     in
   *     let tdecl =
   *       let open Ast_builder.Default in
   *       type_declaration ~loc:extension.ptyext_path.loc
   *         ~name:(Located.map Longident.last_exn extension.ptyext_path)
   *         ~params:extension.ptyext_params
   *         ~private_:Public ~manifest:None ~cstrs:[]
   *         ~kind:(Ptype_variant cds)
   *     in
   *     let fields = self#on_variant ~loc ~is_self_rec ~mutal_decls:[]
   *         tdecl
   *         cds
   *         id
   *     in
   *     let extra_path s = map_longident extension.ptyext_path.txt ~f:(fun _ -> s) in
   *     let inh_params =
   *       prepare_param_triples ~loc
   *         ~inh:(fun ~loc -> self#inh_of_param tdecl)
   *         ~syn:self#syn_of_param
   *         ~default_syn:(self#default_syn ~loc ~extra_path tdecl)
   *         ~default_inh:(self#default_inh ~loc tdecl)
   *         ~extra:(Typ.var ~loc @@
   *                 sprintf "%s_%s" Naming.extra_param_name tdecl.ptype_name.txt)
   *         (map_type_param_names tdecl.ptype_params ~f:id)
   *     in
   *     let parent_plugin_impl =
   *       let params =
   *         self# final_typ_params_for_alias ~loc tdecl
   *           (List.map ~f:fst tdecl.ptype_params)
   *       in
   *       Cf.inherit_ ~loc @@
   *       Cl.apply ~loc
   *         (Cl.constr ~loc (extra_path (self#make_class_name tdecl)) params)
   *         (Exp.sprintf ~loc "%s" Naming.self_arg_name :: (self#apply_fas_in_new_object ~loc tdecl))
   *         (\* TODO: check that apply_fas_... is what we need *\)
   *     in
   *     (\* TODO: It seems that we don't need to inherit interface class for extensible types
   *      * because type parameters are no changing but it require some work to disable this
   *      * generation. So it is postponed *\)
   *     self#wrap_class_definition ~loc [] tdecl ~inh_params
   *       ((self#extra_class_str_members tdecl) @ parent_plugin_impl :: fields)
   *   in
   *   [ clas ] *)


  (* TODO: decide expression of which type should be returned here *)
  (* do_type_gen will return an expression which after being applied
   * to inherited attribute and subject will return synthetized one
   *)
  method do_typ_gen ~loc ~mutal_decls ~is_self_rec t : Exp.t =
    let mutal_names = List.map mutal_decls ~f:(fun t -> t.ptype_name.txt) in
    let access_plugins ~loc e =
      Exp.acc ~loc e @@
      (Ldot (Lident "GT", "plugins"))
    in
    let rec helper ~loc t =
      match t.ptyp_desc with
      | Ptyp_var s -> self#generate_for_variable ~loc s
      | Ptyp_tuple params ->
        self#abstract_trf ~loc (fun einh esubj ->
            self#app_transformation_expr ~loc
              (List.fold_left params
                 ~init:(Exp.send ~loc
                          (access_plugins ~loc
                             (Exp.of_longident ~loc
                                (Ldot (Lident "GT",
                                       Printf.sprintf "tuple%d" (List.length params))))
                          )
                    (* [%expr let open GT in
                     *   [%e  Exp.sprintf "tuple%d" (List.length params)
                     *   ].GT.plugins ] *)
                    self#plugin_name
                       )
                 ~f:(fun left typ ->
                     self#compose_apply_transformations ~loc ~left (helper ~loc typ) typ
                   )
              )
              einh esubj
          )
      | Ptyp_constr (_,_) when is_self_rec t ->
        Exp.ident ~loc Naming.self_arg_name
      | Ptyp_constr ({txt},params) -> begin
          match txt with
          | Lident s when List.mem mutal_names s ~equal:String.equal ->
            (* we should use local trf object *)
            let open Exp in
            app_list ~loc
              (field ~loc
                 (field ~loc (ident ~loc Naming.mut_arg_composite)
                    (lident @@ Naming.trf_function self#plugin_name s))
                 (lident @@ Naming.trf_field ~plugin:self#plugin_name s))
              (List.map params ~f:(self#do_typ_gen ~loc ~is_self_rec ~mutal_decls))
          | _ ->
              let init =
                Exp.(send ~loc
                       (access_plugins ~loc (of_longident ~loc txt))
                       self#plugin_name
                    )
              in
              self#abstract_trf ~loc (fun einh esubj ->
                  self#app_transformation_expr ~loc
                    (List.fold_left params
                       ~init
                       ~f:(fun left typ ->
                           self#compose_apply_transformations ~loc ~left (helper ~loc typ) typ
                         )
                    )
                    einh esubj
                )
        end
        | Ptyp_variant (rows, _, maybe_labels) -> begin
          let oninherit einh esubj typs cident varname =
            self#app_transformation_expr
              (Exp.app_list ~loc
                Exp.(of_longident ~loc @@
                     map_longident cident
                       ~f:(Printf.sprintf "%s_%s" self#plugin_name))
                (List.map typs ~f:(helper ~loc))
              )
              einh esubj
            in
            let onrow lab bindings =
              Exp.app_list ~loc
                (self#on_tuple_constr ~loc ~is_self_rec ~mutal_decls
                   ~inhe:(Exp.sprintf ~loc "inh")
                   (`Poly lab.txt)
                   bindings)
              @@
              List.map bindings ~f:(fun (s,_) -> Exp.ident ~loc s)
            in
            self#abstract_trf ~loc (fun einh esubj ->
              prepare_patt_match_poly ~loc esubj rows maybe_labels
                ~onrow
                ~onlabel:(fun _ _ -> Exp.assert_false ~loc)
                ~oninherit:(oninherit ~loc einh esubj)
            )
          end
        | _ -> failwith "unsupported case in do_typ"
    in
    match self#treat_type_specially t with
    | None -> helper ~loc t
    | Some e -> e

  (* should be used only in concrete plugins  *)
  method treat_type_specially t = None

  method compose_apply_transformations ~loc ~left right typ =
    Exp.app ~loc left right

  method virtual app_gcata: loc:loc -> Exp.t -> Exp.t
  method virtual make_typ_of_self_trf: loc:loc -> type_declaration -> Typ.t
  (* method virtual default_syn  : loc:loc -> Ppxlib.type_declaration -> Typ.t *)
  method virtual default_inh : loc:loc -> Ppxlib.type_declaration -> Typ.t

  method virtual make_RHS_typ_of_transformation: loc:AstHelpers.loc ->
         ?subj_t:Typ.t -> ?syn_t:Typ.t -> type_declaration -> Typ.t
end

(** Base plugin class where transformation functions doesn't use inherited
    attribute.
    See {!Show} and {!Gmap} plugin for examples.
  *)
class virtual no_inherit_arg = object(self: 'self)

  method virtual plugin_name: string
  method virtual default_syn : loc:loc ->
      ?extra_path:(Ppxlib__.Import.string -> longident) ->
      Ppxlib.type_declaration -> Typ.t
  method virtual default_inh : loc:loc -> Ppxlib.type_declaration -> Typ.t
  method virtual syn_of_param: loc:loc -> string -> Typ.t
  method virtual inh_of_param: type_declaration -> string -> Typ.t

  method use_tdecl = Typ.use_tdecl

  (* almost the same as `make_typ_of_class_argument` *)
  method make_typ_of_self_trf ~loc tdecl =
    let is_poly = is_polyvariant_tdecl tdecl in
    let openize_poly typ =
      if is_poly then Typ.openize ~loc (Typ.from_caml typ)
      else Typ.from_caml typ
    in

    let subj_t = openize_poly @@ using_type ~typename:tdecl.ptype_name.txt tdecl in
    let syn_t  = self#default_syn ~loc tdecl in
    Typ.(arrow ~loc subj_t @@ syn_t)

  (* val name: <fa> -> <fb> -> ... -> <fz> -> <_not_ this>
   *   fot a type ('a,'b,....'z) being generated
   **)
  (* method virtual make_typ_of_class_argument: *)

  method make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      k @@ (fun arg -> chain (Typ.arrow ~loc subj_t syn_t) arg)

  method make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t
        ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t ~default:(self#default_syn ~loc tdecl) in
    Typ.arrow ~loc subj_t syn_t

  method abstract_trf ~loc k =
    Exp.fun_ ~loc (Pat.sprintf ~loc "subj") @@
    k (Exp.assert_false ~loc) (Exp.ident ~loc "subj")
    (* [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]] *)

  method app_transformation_expr ~loc trf (inh: Exp.t) subj =
    (* we ignore inherited argument by default *)
    Exp.app ~loc trf subj

  method app_gcata ~loc egcata =
    Exp.app ~loc egcata (Exp.unit ~loc)

  method on_record_constr: loc:loc ->
    is_self_rec:(core_type -> bool) ->
    mutal_decls:type_declaration list ->
    inhe:Exp.t ->
    [ `Normal of string | `Poly of string ] ->
    (string * string * core_type) list ->
    label_declaration list -> 
    Exp.t = fun  ~loc ~is_self_rec ~mutal_decls ~inhe _ _ _ ->
    failwithf "handling record constructors in plugin `%s`" self#plugin_name ()

end

(** Base plugin class where transformation functions receive inherited attribute for
    type parameter *)
class virtual with_inherit_arg = object(self: 'self)
  inherit no_inherit_arg as super

  method wrap_tr_function_str ~loc (tdecl: type_declaration) make_gcata_of_class =
    (* [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj] *)
    let body = make_gcata_of_class in
    Exp.fun_list ~loc [ Pat.sprintf ~loc "the_init"; Pat.sprintf ~loc "subj"] @@
    Exp.app_list ~loc
      (Exp.of_longident ~loc (Ldot (Lident "GT", "transform1_gc")))
      [ Exp.sprintf ~loc "gcata_%s" tdecl.ptype_name.txt (* TODO: name *)
      ; body
      ; Exp.sprintf ~loc "the_init"
      ; Exp.sprintf ~loc "subj"
      ]

  method! make_typ_of_self_trf ~loc tdecl =
    Typ.arrow ~loc (self#default_inh ~loc tdecl) (super#make_typ_of_self_trf ~loc tdecl)

  (* val name: <fa> -> <fb> -> ... -> <fz> -> <_not_ this>
   *   fot a type ('a,'b,....'z) being generated
   **)

  method make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let inh_t = self#inh_of_param tdecl name in
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      k @@ (fun arg -> chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t) arg)

  method! make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t  ~default:(self#default_syn ~loc tdecl) in
    Typ.arrow ~loc (self#default_inh ~loc tdecl)
      (super#make_RHS_typ_of_transformation ~loc ~subj_t ~syn_t tdecl)

  method! abstract_trf ~loc k =
    Exp.fun_list ~loc [ Pat.sprintf ~loc "inh"; Pat.sprintf ~loc "subj" ]  @@
    k (Exp.ident ~loc "inh") (Exp.ident ~loc "subj")
    (* [%expr fun inh subj -> [%e k [%expr inh ] [%expr subj]]] *)

  method! app_transformation_expr ~loc trf inh subj =
    (* we ignore inherited argument by default *)
    Exp.app_list ~loc trf [inh; subj]

  method! app_gcata ~loc egcata = egcata

end

end
