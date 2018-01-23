(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core
open Ppx_core.Ast_builder.Default
open Printf
open Longident
open Asttypes
open Ast_helper
open GtHelpers
let (@@) = Caml.(@@)

let deriver = "gt"

type supported_derivers = { gt_show: bool; gt_gmap: bool }
                          (*
(*
let parse_options options =
  List.fold_left ~f:(fun acc (name,expr) ->
    match name with
    | "show" -> {acc with gt_show = true}ук
    | "gmap" -> {acc with gt_gmap = true}
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)
    ~init:{ gt_show=false; gt_eq=false; gt_gmap=false }
    options
  *)
let argn = Printf.sprintf "a%d"

let default_params ?(loc=Location.none) root_type =
  (* converts 'a, 'b to
           [ 'a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn ]
   *)
  let ps = root_type.ptype_params |>
             List.map ~f:(fun (t,_) ->
                       match t.ptyp_desc with
                       | Ptyp_var n -> Typ.([var n; var @@ "i"^n; var @@ "s"^n])
                       | _ -> raise_errorf "default_params: can't construct"
                      )
  in
  let ps = List.concat ps in
  let ps = ps @ [ [%type: 'inh]; [%type: 'syn] ] in
  invariantize ps

(* Used when we need to check that type we working on references himself in
  it's body *)
let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (* Pprintast.core_type Format.std_formatter (Obj.magic typ);
  Format.pp_force_newline Format.std_formatter ();
  Format.pp_print_flush Format.std_formatter (); *)

  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Lident xxx},_) ->
    let b = (Caml.(=) xxx  tdecl.ptype_name.txt) in
    (* printf "xxx = %s, tdecl.ptype_name.txt = %s, %b\n%!" xxx tdecl.ptype_name.txt b; *)
    b
  | _ ->
    false
  )

let make_params_lambda_generic ?(loc=Location.none) ~root_type namer expr  =
  map_type_param_names root_type.ptype_params ~f:id |>
  List.fold_right ~f:(fun name acc ->
    [%expr fun [%p Pat.var @@ (mknoloc @@ namer name)] -> [%e acc ] ]
  )  ~init:expr

let make_params_lambda_a  = make_params_lambda_generic (fun name -> name)
let make_params_lambda_fa = make_params_lambda_generic ((^)"f")

let wrap_with_fa ?(use_lift=false) ?(add_subj=true)
    ~root_type func lasts =
  let loc = root_type.ptype_loc in
  let right =
    map_type_param_names root_type.ptype_params ~f:(fun name ->
      if use_lift then [%expr GT.lift [%e Exp.ident ("f"^ name) ]]
      else  Exp.ident ("f"^ name)
    )
  in
  let right = right @ (List.map (fun typ ->  typ) lasts) in
  let right = Exp.apply func (nolabelize right) in
  let right = if add_subj then[%expr fun subj  -> [%e right] subj ] else right in
  make_params_lambda_fa ~root_type right

(* There we generate three lists
  1. Method signature for tt class type
  2. Virtual methods for t class structure
  3. Virtual methods for t class signature
  *)
let generate_some_methods ~typename ?(t_virtual=false) root_type constrs =
  let loc = root_type.ptype_loc in
  let t_typename = "t_" ^ typename in
  let xs = List.map constrs ~f:(fun { pcd_name = { txt = name' }; pcd_args } ->
    let Pcstr_tuple pcd_args = pcd_args in
    (* for every type constructor *)
    let constr_name = "c_" ^ name' in
    (* type that will be used for derivied type *)
    let arg_for_myself =
      [%type: ('inh,
              [%t using_type ~typename root_type],
              'syn,
              [%t params_obj root_type]) GT.a ]
    in
    let args2 = pcd_args |> List.map ~f:(fun ({ ptyp_desc; _ } as typ) ->
      match ptyp_desc with
      | _ when are_the_same typ root_type -> arg_for_myself
      | Ptyp_var a  ->
          [%type: ([%t Typ.var @@ "i"^a],
                   [%t typ ],
                   [%t Typ.var @@ "s"^a],
                   [%t params_obj root_type]) GT.a ]
      | Ptyp_constr _ -> typ
      (* | Ptyp_constr ({txt=Lident "int"; _},[]) ->              [%type: int]
      | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) -> [%type: GT.int]
      | Ptyp_constr _ ->
          [%type: ([%t Typ.var @@ "inh"],
                   [%t typ],
                   [%t Typ.var @@ "syn"],
                   [%t params_obj root_type ]) GT.a ] *)
      | _ -> raise_errorf "Some cases are not supported when we look at constructor's params"
    )
    in
    let args2 =
      (* some additional arguments should be prepended to list of types
         generated from constructor arhuments *)
      (Typ.var "inh") ::
      arg_for_myself ::
      args2
    in

    let ts = List.fold_right ~f:(Typ.arrow Nolabel) args2 ~init:(Typ.var "syn") in
    ( Ctf.method_ constr_name Public Concrete ts,
      (* Cf.method_  (mknoloc constr_name) Public (Cfk_virtual ts)  *)
      Cf.method_  constr_name Public (Cfk_virtual ts),
      Ctf.method_ constr_name Public Virtual ts
    )
  )
  in
  let (tts, ts, ts_sigs) = List.split3 xs in
  let meth_main_mapper_typ =
    let ts = List.map (fun (t,_) -> arr_of_param t) root_type.ptype_params in
    let init =
      [%type: 'inh -> [%t using_type ~typename root_type] -> 'syn ]
    in
    List.fold_right ~f:(fun (_,_,x) acc -> Typ.arrow Nolabel x acc) ts ~init
  in
  let tts = tts @
            [ Ctf.method_ ("t_" ^ typename) Public Concrete meth_main_mapper_typ
            ]
  in

  let main_mapper_body =
    wrap_with_fa ~use_lift:false
      [%expr GT.transform [%e Exp.ident typename]]
      [  [%expr this] ]
  in
  let ts = ts @ [ Cf.method_ t_typename Public
                    (Cfk_concrete (Fresh, main_mapper_body ~root_type))
                ]
  in
  let ts_sigs = ts_sigs @
    [ Ctf.method_ t_typename Public Concrete
        meth_main_mapper_typ
    ]
  in
  (tts, ts, ts_sigs)

module MakeMeta = struct
  let sig_tt_for_algebraic ~params ~typename cases_list =
    failwith "not implemented"

  let standart_type_prefix ?(loc=Location.none) ?(itself = [%type: 'type_itself])
      ?(gt_a_for_self = [%type: 'gt_a_for_self]) () =
    [ [%type: 'inh]; [%type: 'syn]; [%type: 'tpoT]; itself; gt_a_for_self ]

  let t_params ~params =
    let poly_params = List.filter_map params ~f:(fun (p,_) ->
      match p.ptyp_desc with
      | Ptyp_var name -> Some name
      | _ -> None
      )
    in

    if Int.(List.length params <> List.length poly_params)
    then failwith "constructor declaration has not vars in a params";
    let ps = standart_type_prefix () in
    let ps = ps @
      List.map (fun s -> Typ.var @@ "gt_a_for_"^s) poly_params
    in
    invariantize ps

  let str_tt_for_alias ?(loc=Location.none) ~params ~tt_name ~manifest =
    let params = t_params params in
    let meths = [] in
    (* Not  finished *)
    Str.class_type [Ci.mk ~virt:Virtual ~params
                    (mknoloc tt_name) @@
                  Cty.signature (Csig.mk [%type: _] meths) ]

  let str_tt_for_algebraic ?(loc=Location.none) ~params ~tt_name cases_list =
    let params = t_params params in
    let meths = List.map cases_list ~f:(fun ctr ->
      if Option.is_some ctr.pcd_res
      then failwith "GADT not supported";
      match ctr.pcd_args with
      | Pcstr_tuple xs ->
        let xs2 = List.map xs ~f:(fun arg -> match arg.ptyp_desc with
          | Ptyp_var name -> Typ.var (sprintf "gt_a_for_%s" name)
          | Ptyp_constr _ -> arg
          | _ -> assert false
        )
        in
        let methname = "c_" ^ ctr.pcd_name.txt in
        let xs2 = [%type: 'inh] :: (make_gt_a_typ ()) :: xs2 in
        Ctf.method_ methname Public Virtual @@
          List.fold_right xs2 ~init:[%type: 'syn] ~f:(Typ.arrow Nolabel)
      | Pcstr_record _ -> failwith "not supported"
    ) in
    Str.class_type [Ci.mk ~virt:Virtual ~params
                    (mknoloc tt_name) @@
                  Cty.signature (Csig.mk [%type: _] meths) ]

  (* declared metaclass for an algebraic type *)
  let str_class_for_algebraic ?(loc=Location.none) ~params ~t_name ~parent_name =
    let params = t_params params in
    Str.single_class ~params  ~name:t_name ~pat:[%pat? (self: 'self)]
                    [ Cf.constraint_ [%type: 'self] @@
                        Typ.class_ (mknoloc (Lident parent_name)) (List.map ~f:fst params)
                    ]

  (* let str_t_for_alias ~params ~t_name ~meta_tt_name = *)

  let str_meta_gcata_for_alias ~root_type ~name ~manifest =
    let loc = root_type.ptype_loc in
    match manifest.ptyp_desc with
    | Ptyp_constr ({txt=parent_name;_}, args) ->
        let left,right = List.fold_right args ~init:([ [%pat? fix_eta] ],[ [%expr fix_eta ] ]) ~f:(fun typ (accl,accr) ->
          match typ with
          | { ptyp_desc=Ptyp_var name } ->
              let pat = Pat.var @@ mknoloc @@ sprintf "on_%s_arg" name in
              let arg = Exp.ident @@ sprintf "on_%s_arg" name in
              (pat::accl, arg::accr)
          | [%type: int ]
          | [%type: char ]
          | [%type: string ] -> (accl, [%expr (fun x -> x) ]:: accr)
          | t when are_the_same t root_type ->  (accl, [%expr (fun x -> x) ]:: accr)
          | _  -> failwith "not implemented"
          )
        in
        let right = List.map right ~f:(fun x -> (Nolabel,x)) in
        let parent_gcata = Exp.ident_of_long @@ mknoloc @@
          affect_longident ~f:(sprintf "%s_meta_gcata") parent_name
        in

        [%stri
          let [%p (Pat.var @@ mknoloc name) ] =
            [%e Exp.fun_list ~args:left
                (if List.empty right then parent_gcata
                 else Exp.apply parent_gcata right)
            ]
            (* [%e make_params_lambda_fa ~root_type
              [%expr
              fun tpo trans initial_inh subj ->
                let self = [%e make_params_app_fa (Exp.ident @@ lid typename_meta_gcata)
                                                   [ [%expr tpo]; [%expr trans] ] ]
                in
                [%e match_body]
            ]] *)
        ]
    | _ -> assert false

  let str_meta_clas_for_alias ~root_type ~name ~manifest =
    let loc = root_type.ptype_loc in
    let params = t_params ~params:root_type.ptype_params in
    match manifest.ptyp_desc with
    | Ptyp_constr ({txt=ident;_}, args) ->
      let inh_types =
        let ps = List.filter_map args ~f:(function
          | {ptyp_desc=Ptyp_var name;} -> Some (Typ.var @@ sprintf "gt_a_for_%s" name)
          | [%type: string] as t -> Some t
          | [%type: char] as t -> Some t
          | [%type: int] as t -> Some t
          | t when are_the_same t root_type ->
              (* This 'gt_a_for_self can be wrong when we have different occurences of the derived type in itself
                 For example:
                    ('a,'b) t = ( ..., ('a,'b) t, ...., ('b,'a) t, ... ) gt
              *)
              Some [%type: 'gt_a_for_self]
          | typ -> failwith
                     (sprintf "%s %d: Don't know what to do about the type `%s`"
                        Caml.__FILE__ Caml.__LINE__ (string_of_core_type typ))
          ) in
        let ps = [ [%type: 'inh]; [%type: 'syn]; [%type: 'tpoT]; [%type: 'type_itself] ; [%type: 'gt_a_for_self] ] @ ps in
        ps
      in
      let parent_name = affect_longident ident ~f:(fun s -> s^"_meta_t") in
      Str.single_class ~params ~name
                      [ Cf.inherit_ (Cl.constr (mknoloc parent_name) inh_types) 
                      (* ; Cf.constraint_ [%type: 'self] @@
                          Typ.class_ (mknoloc parent_name) (List.map ~f:fst params) *)
                      ]

    | _ -> failwith (sprintf "str_meta_clas_for_alias fails for type with name '%s'" name)

  let str_meta_gcata_for_algebraic ~root_type ~typename ~gcata_name make_params_app_fa constrs =
    let loc = root_type.ptype_loc in
    let typename_meta_gcata = typename^"_meta_gcata" in
    let match_body =
      Exp.match_ (Exp.ident "subj") @@
      List.map constrs ~f:(fun { pcd_name = { txt = name' }; pcd_args } ->
        let Pcstr_tuple pcd_args = pcd_args in
        let argnames = List.mapi (fun n _ -> sprintf "p%d" n) pcd_args in
        let args_tuple =
          match argnames with
          | [] -> None
          | [single_arg] -> Some (Pat.var @@ mknoloc single_arg)
          | _ -> Some (Pat.tuple @@ List.map (fun s-> Pat.var @@ mknoloc s) argnames)
        in

        let app_args = List.map2 (fun argname arg ->
          match arg.ptyp_desc with
            | _ when are_the_same arg root_type -> [%expr
              GT.make self [%e Exp.ident argname] tpo ]
          | Ptyp_var v ->
              Exp.(apply (ident @@ "f"^v) [ (Nolabel, ident argname)] )

          | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) ->
              [%expr [%e Exp.ident argname]]
          | Ptyp_constr ({txt=(Lident "int"); _},[]) ->
              [%expr [%e Exp.ident argname]]
          | Ptyp_constr _ ->
              [%expr [%e Exp.ident argname]]
          | _ -> raise_errorf "Some cases are not supported when generating application in gcata"
        ) argnames pcd_args
        in

        Exp.case (Pat.construct (Located.lident ~loc name') args_tuple) @@
        Exp.(apply (send (ident "trans") ("c_"^name') )
             @@ List.map ~f:(fun x -> (Nolabel,x))
               ([ [%expr initial_inh]; [%expr (GT.make self subj tpo)] ] @ app_args)
            )
      )
    in
    [%stri
      let rec [%p (Pat.var @@ mknoloc gcata_name) ] =
        [%e make_params_lambda_fa ~root_type
          [%expr
          fun tpo trans initial_inh subj ->
            let self = [%e make_params_app_fa (Exp.ident typename_meta_gcata)
                                               [ [%expr tpo]; [%expr trans] ] ]
            in
            [%e match_body]
        ]]
    ]


end

let make_tt_class_type ~root_type ~typename ~typename_meta_tt ~typename_tt tt_methods =
  (* TODO: fix derty hack: there we suppose that last element of tt_methods
    is a transformer for a whole type
  *)

  let loc = root_type.ptype_loc in
  let inheritF =
    let params = [ [%type: 'inh]; [%type: 'syn] ] in
    let itself = Typ.constr (lid typename) (List.map ~f:fst root_type.ptype_params) in
    let tpoT = Typ.alias (params_obj root_type) "tpoT" in
    let gtas = List.map root_type.ptype_params ~f:(fun (t,_) ->
      match t.ptyp_desc with
      | Ptyp_var name ->
          let inh = Typ.var (sprintf "i%s" name) in
          let syn = Typ.var (sprintf "s%s" name) in
          make_gt_a_typ ~inh ~itself:t ~syn ()
      | _ -> assert false
    ) in
    Ctf.inherit_ @@ Cty.constr (lid typename_meta_tt)
      ([tpoT; itself] @ gtas @ params)

  in
  let methods = [ inheritF; List.last_exn tt_methods] in

  Str.class_type [Ci.mk ~virt:Virtual ~params:(default_params root_type)
                    (mknoloc typename_tt) @@
                  Cty.signature (Csig.mk [%type: _] methods) ]

let make_params_longarrow ~root_type typ =
  let loc = root_type.ptype_loc in
   List.fold_right ~f:(fun ({ptyp_desc},_) acc ->
    match ptyp_desc with
    | Ptyp_var n ->
       Typ.(arrow Nolabel
                  [%type: [%t var@@ "i"^n] -> [%t var n] -> [%t var @@ "s"^n]]
                  acc)
    | _ -> assert false) root_type.ptype_params ~init:typ

let subclass_obj ~root_type typename_tt =
  let loc = root_type.ptype_loc in
  (* makes ('a,'ia,'sa,...,'inh,'syn)#typename_tt  *)
  Typ.class_ (lid typename_tt) @@ List.map ~f:fst (default_params root_type)

let any_typ ?(loc=Location.none) () = [%type: _]

(* let gt_repr_typ_wrap ~typename ~root_type arg =
  let typename_tt = typename ^ "_tt" in
  let tail = [%type: 'inh -> [%t using_type ~typename root_type ] -> 'syn ] in
  let subclass = subclass_obj typename_tt ~root_type in
  [%type: ([%t make_params_longarrow ~root_type
              [%type: [%t subclass] -> [%t tail]]],
           [%t arg]) GT.t ]
*)
let inherit_field_gen ~name ~root_type ~inh ~synh ~holder ~synh_root wrap =
  let loc = root_type.ptype_loc in
  let synhs, types = List.split @@
    map_type_param_names root_type.ptype_params
      ~f:(fun name -> (synh name, [Typ.var name; inh name; synh name; holder name ]) )
  in
  let types = List.concat types in
  wrap Typ.([%type: unit] :: (synh_root root_type synhs) :: [%type: 'tpoT] :: types)

let inherit_cf ?args ~name ~root_type ~inh ~synh ~holder ~synh_root () =
  let loc = root_type.ptype_loc in
  inherit_field_gen ~name ~root_type ~inh ~synh ~holder ~synh_root
    (fun t ->
      let class_expr = Cl.constr (lid (Lident name)) t in
      let class_expr = match args with
        | None -> class_expr
        | Some xs -> Cl.apply class_expr xs
      in
      Cf.inherit_ class_expr)

let sig_of_type ~options ~path ({ ptype_params=type_params } as root_type) =
  let loc = root_type.ptype_loc in

  let { gt_show; gt_gmap } = options in

  let typename    = root_type.ptype_name.txt in
  let typename_t  = typename ^ "_t"  in
  let typename_tt = typename ^ "_tt" in

  match root_type.ptype_kind with
  | Ptype_abstract -> begin
      match root_type.ptype_manifest with
      | Some [%type: int] -> []
        (* [ make_primitive_bunch root_type.ptype_name.txt [%expr GT.(int.gcata)]] @
        show_decls root_type @
        gmap_decls root_type @
        [derivers_bunch] *)
      | _ -> raise_errorf "not implemented?6"
    end
  | Ptype_variant constrs ->
    let (tt_methods, t_methods, t_meth_sigs) =
      generate_some_methods ~typename ~t_virtual:true root_type constrs  in
    let ans =
      [ Sig.class_type [Ci.mk ~virt:Virtual ~params:(default_params root_type)
                        (mknoloc typename_tt) @@
                      Cty.signature (Csig.mk [%type: _] tt_methods) ]
      (* ; Sig.value @@ Val.mk (mknoloc typename)
          (gt_repr_typ_wrap ~root_type ~typename [%type: unit]) *)
      ; Sig.class_
          [ Ci.mk ~virt:Virtual ~params:(default_params root_type)
              (mknoloc typename_t) @@
              Cty.signature
                (Csig.mk (any_typ ()) t_meth_sigs)
          ]

      ]
    in
    (* let ans = if not gt_show then ans else ans @ (show_decls root_type) in *)
    (* TODO: add gmap here *)

    let derivers_bunch =
      let wrap_meth mname cname =
        let typname = root_type.ptype_name.txt in
        let body =
          wrap_with_fa ~use_lift:true [%expr GT.transform [%e Exp.ident typname]] ~root_type
            [ Exp.new_ @@ lid cname; [%expr () ] ]
        in
        Cf.method_ mname Public (Cfk_concrete (Fresh, body))
      in
      let gcata_part =
        let args2 = List.map (fun (t,_) -> let (_,_,x) = arr_of_param t in x) type_params in
        List.fold_right args2
          ~f:(Typ.arrow Nolabel)
          ~init:[%type: [%t subclass_obj ~root_type (Lident typename_tt) ] ->
                    'inh -> [%t using_type ~typename root_type ] -> 'syn ]
      in
      let plugins_part =
        let for_show =
          if not gt_show then [] else
          let xs = List.map (fun (typ,_) -> [%type: [%t typ] -> string]) type_params in
          [ ("show", [],
            List.fold_right ~f:(fun x acc -> [%type: [%t x] -> [%t acc]]) xs
              ~init:[%type: [%t using_type ~typename root_type ] -> string ])
          ]
        in
        let for_gmap = [] in
        Typ.object_ (for_show @ for_gmap) Closed
      in
      Sig.value (Val.mk (mknoloc typename) [%type: ([%t gcata_part], [%t plugins_part]) GT.t])
    in

    ans @ [derivers_bunch]
  | _ -> raise_errorf "Some cases are not supported"

module type Plugin =
  sig
    val name : string

    (* Extended parameters that must be appended to class parameters *)
    val extra_params : type_declaration -> (core_type * variance) list

    val inh  : ?loc:Ppx_core.Location.t -> string -> core_type
    val synh : ?loc:Ppx_core.Location.t -> string -> core_type

    val synh_root : type_declaration -> core_type list -> core_type

    (* some stuff for abstract types and aliases *)
    val core : core_type -> class_expr
    val meta_for_alias : name:string -> root_type:type_declaration -> manifest:core_type -> structure_item
    val for_alias : name:string -> root_type:type_declaration -> manifest:core_type -> structure_item

    (* used for algebraic datatypes *)
    val constructor : type_declaration -> constructor_declaration -> class_field
  end

let plugin_decls (module P: Plugin) root_type =
  let loc = root_type.ptype_loc in
  let typename    = root_type.ptype_name.txt in
  let typename_t  = typename ^ "_t"  in
  let plugin_name = P.name ^ "_" ^ typename in
  let plugin_meta_t = P.name ^ "_meta_" ^ typename in
  let param_names = map_type_param_names root_type.ptype_params ~f:(fun x -> x) in

  match root_type.ptype_kind with
  | Ptype_abstract -> (match root_type.ptype_manifest with
    | None -> failwith "we can't generate anything for really abstract types"
    | Some manifest ->
        [ P.meta_for_alias ~name:plugin_meta_t ~root_type ~manifest
        ; P.for_alias ~name:plugin_name ~root_type ~manifest
        ]
        (* [Str.class_ [Ci.mk ~virt:Concrete ~params:[] (mknoloc plugin_name) (P.core manifest) ]] *)
      (* P.core manifest *)
    )
  | Ptype_variant constrs ->
    let holder s = Typ.var @@ sprintf "%s_holder" s in
    let type_param_names = map_type_param_names root_type.ptype_params ~f:(fun x -> x) in

    let plugin_meta_class =
      let params = List.map type_param_names ~f:(fun name ->
          [ Typ.var name; holder name ]
        ) |> List.concat
      in
      let params = [Typ.var "tpoT"] @ params @ [ [%type: 'self_holder] ] in
      let params = invariantize params in
      let params = params @ (P.extra_params root_type)  in

      let inherit_f = inherit_cf ~name:typename_t ~root_type ~inh:P.inh ~synh:P.synh ~synh_root:P.synh_root ~holder ()
      in
      let body =
        (* (inherit_cf ~name:typename_t ~root_type ~inh:P.inh ~synh:P.synh ~synh_root:P.synh_root ~holder) :: *)
        List.map (fun constr -> P.constructor root_type constr) (List.rev constrs)
      in
      let class_expr = Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this") (inherit_f::body) ) in
      let class_expr =
        let for_args = map_type_param_names root_type.ptype_params ~f:(fun s -> Pat.var @@ mknoloc @@ "for_"^s) in
        let for_args = for_args @ [ for_me_patt ~loc () ] in
        Cl.fun_list for_args class_expr
      in

      Str.class_ [Ci.mk ~virt:Concrete ~params
                      (mknoloc plugin_meta_t)
                      class_expr
                   ]
    in
    let plugin_class =
      let inherit_f =
        let params =
          List.map type_param_names ~f:(fun name ->
            let p = Typ.var name in
            [ p; make_gt_a_typ ~inh:[%type: unit] ~itself:p ~syn:(P.synh "")  ()])
          |> List.concat
        in
        let params =
          (Typ.alias (params_obj ~inh:(fun _ -> Typ.ground "unit") ~syn:(fun _ -> Typ.ground "string") root_type) "tpoT" )
          :: params
        in
        let params = params @ [ using_type ~typename root_type] in
        let class_expr = Cl.constr (Located.lident ~loc plugin_meta_t) params in
        let args = nolabelize @@ map_type_param_names root_type.ptype_params
          ~f:(fun name ->
                let name = "p" ^ name in
                [%expr fun [%p Pat.var (lid name) ] -> [%e Exp.ident name].GT.fx ()])
        in
        let args = args @ [ Nolabel, for_me_expr () ] in
        let class_expr = match args with
        | [] -> class_expr
        | xs -> Cl.apply class_expr xs
        in
        Cf.inherit_  class_expr 
      in
      Str.class_ [Ci.mk ~virt:Concrete ~params:(root_type.ptype_params @ (P.extra_params root_type))
                    (mknoloc plugin_name)
                    (Cl.fun_ Nolabel None (for_me_patt ~loc ()) @@
                      Cl.structure (Cstr.mk (Pat.any ()) [inherit_f]))
                 ]
    in
    [ plugin_meta_class
    ; plugin_class
    ]
  | _ -> failwith "Some shit happend"

let tpo_obj ~root_type =
  let loc = root_type.ptype_loc in
  let tpo_meths =
    let f ({ptyp_desc; _},_) =
      match ptyp_desc with
      | Ptyp_var v -> Cf.method_ v Public (Cfk_concrete (Fresh, Exp.ident ("f"^v)))
      | _ -> raise_errorf "Some cases are not supported when creating tpo methods"
    in
    List.map f root_type.ptype_params
  in
  Exp.object_ (Cstr.mk (Pat.any ()) tpo_meths)

let make_gcata ~root_type ~name ~metaname =
  let loc = root_type.ptype_loc in
  let transformer_name = ((^)"f") in
  let poly_param_names = map_type_param_names (fun x -> x) root_type.ptype_params in
  let meta_gcata_args =
    (List.map (fun name ->
                  [%expr fun x -> GT.make [%e Exp.ident @@ transformer_name name]
                                          x parameter_transforms_obj]) poly_param_names
    ) @ [ [%expr parameter_transforms_obj]; [%expr transformer]; [%expr initial_inh]; [%expr subj] ]
  in
  let meta_gcata_args = List.map (fun x -> (Nolabel,x)) meta_gcata_args in
  let arg_pats = [ [%pat? transformer]; [%pat? initial_inh]; [%pat? subj] ] in
  let arg_pats = List.map poly_param_names ~f:(fun name -> Pat.var @@ mknoloc @@ transformer_name name) @ arg_pats in
  Str.value Nonrecursive [Vb.mk
    Pat.(var @@ mknoloc name) @@
      Exp.fun_list ~args:arg_pats
      [%expr
        let parameter_transforms_obj = [%e tpo_obj ~root_type] in
        [%e Exp.(apply (ident metaname)) meta_gcata_args ]
      ]
    ]

let str_of_type ~options ~path ({ ptype_params=type_params } as root_type) =
  let loc = root_type.ptype_loc in
  let { gt_show; gt_gmap } = options in
  (* let _quoter = Ppx_deriving.create_quoter () in *)
  (* let path = Ppx_deriving.path_of_type_decl ~path root_type in *)

  let typename    = root_type.ptype_name.txt in
  let typename_t  = typename ^ "_t"  in
  let typename_tt = typename ^ "_tt" in
  let typename_meta_t  = typename ^ "_meta_t" in
  let typename_meta_tt = typename ^ "_meta_tt" in
  let typename_meta_gcata = typename ^ "_meta_gcata" in
  let typename_gcata = typename ^ "_gcata" in
  (* let _t_typename  = "t_" ^ typename  in *)

  let show_typename_t = "show_" ^ typename in
  let gmap_typename_t = "gmap_" ^ typename in

  let derivers_bunch =

    let wrap_meth mname (cname:string) =
      (* let typname = root_type.ptype_name.txt in *)
      let self_shower =
        let args = map_type_param_names root_type.ptype_params
            ~f:(fun name -> Exp.ident @@ sprintf "f%s" name)
        in
        [%expr  [%e Exp.new_ (Located.lident ~loc cname) ]
                [%e List.fold_left ~init:(Exp.(send (ident "self")) mname) args
                      ~f:(fun acc x -> [%expr [%e acc] [%e x]])
                ]]
      in
      let body =
        wrap_with_fa ~use_lift:true (Exp.ident typename_gcata) ~root_type
          [ self_shower; [%expr () ] ]
      in
      Cf.method_ mname Public (Cfk_concrete (Fresh, body))
    in
    [%stri let [%p Pat.var @@ mknoloc typename] =
      { GT.gcata = [%e Exp.(ident typename_gcata) ]
      ; GT.plugins = [%e Exp.object_ @@ Cstr.mk ~self:(Pat.var (mknoloc "self")) @@
        (if gt_show then [wrap_meth "show" show_typename_t] else []) @
        (if gt_gmap then [wrap_meth "gmap" gmap_typename_t] else []) @
        []
        ]
      }
    ]
  in


  (* let derivers_bunch =
    [%stri let () = () ]
  in *)

  let make_primitive_bunch name prim_gcata =
    [%stri let [%p Pat.var @@ mknoloc name ] = { GT.gcata = [%e prim_gcata]; GT.plugins = [] }]
  in

  let show_decls = if gt_show then plugin_decls (module Show: Plugin) root_type else [] in
  let gmap_decls =
    []
    (* if gt_gmap then plugin_decls (module Gmap: Plugin) root_type else []  *)
  in

  match root_type.ptype_kind with
  | Ptype_abstract -> begin
      match root_type.ptype_manifest with
      | None -> failwith "can't be supported"
      | Some manifest ->
          [ MakeMeta.str_meta_gcata_for_alias ~root_type ~name:typename_meta_gcata ~manifest
          (* gcata for type alias is declared the same as for normal algebraic type *)
          ; make_gcata ~root_type ~name:typename_gcata ~metaname:typename_meta_gcata
          ; MakeMeta.str_tt_for_alias ~loc
              ~params:root_type.ptype_params ~tt_name:typename_meta_tt ~manifest
          ; MakeMeta.str_meta_clas_for_alias ~root_type ~name:typename_meta_t ~manifest
          (* t class is omitted mecause it seems that we don't need it *)
          (* ; MakeMeta.str_t_for_alias ~params:root_type.ptype_params ~t_name:typename_t ~meta_tt_name:typename_meta_ *)
          ] @ show_decls @ [derivers_bunch]
      | Some [%type: int] ->
        [ make_primitive_bunch root_type.ptype_name.txt [%expr GT.(int.gcata)]] @
        gmap_decls @
        show_decls @ [derivers_bunch]
      | _ -> raise_errorf "%s %s" "not implemented?5" root_type.ptype_name.txt
    end
  | Ptype_variant constrs ->
      let _make_params_app_with_lift first lasts =
        let first_part =
          List.map (function ({ptyp_desc; _ },_) ->
            match ptyp_desc with
            | Ptyp_var name -> (Nolabel, [%expr GT.lift [%e Exp.ident name]])
            | _ -> assert false) root_type.ptype_params
        in
        let second_part = List.map (fun typ -> (Nolabel, typ)) lasts in
        Exp.apply first (first_part @ second_part)
      in
      let make_params_app_namer ?(use_lift=false) ~namer first lasts =
        let wrap name =
          if use_lift then [%expr GT.lift [%e namer name]]
          else  namer name
        in
        let first_part =
          List.map (function ({ptyp_desc; _ },_) ->
            match ptyp_desc with
            | Ptyp_var name -> (Nolabel, wrap name)
            | _ -> assert false) root_type.ptype_params
        in
        let second_part = List.map (fun typ -> (Nolabel, typ)) lasts in
        Exp.apply first (first_part @ second_part)
      in

      let make_params_app_fa =
        make_params_app_namer ~namer:(fun name -> Exp.ident ("f"^name))
      in

      (* let gt_repr_typ = gt_repr_typ_wrap ~typename ~root_type [%type: unit] in *)
      let gt_repr_body =
        let typename_gcata = typename^"_gcata" in
        let tpo_meths =
          let f ({ptyp_desc; _},_) =
            match ptyp_desc with
            | Ptyp_var v -> Cf.method_ v Public (Cfk_concrete (Fresh, Exp.ident("f"^v)))
            | _ -> raise_errorf "Some cases are not supported when creating tpo methods"
          in
          List.map f root_type.ptype_params
        in
        let tpo = Exp.object_ (Cstr.mk (Pat.any ()) tpo_meths ) in
        let match_body =
          Exp.match_ (Exp.ident "subj") @@
          List.map constrs ~f:(fun { pcd_name = { txt = name' }; pcd_args } ->
            let Pcstr_tuple pcd_args = pcd_args in
            let argnames = List.mapi (fun n _ -> sprintf "p%d" n) pcd_args in
            let args_tuple =
              match argnames with
              | [] -> None
              | [single_arg] -> Some (Pat.var @@ mknoloc single_arg)
              | _ -> Some (Pat.tuple @@ List.map (fun s-> Pat.var @@ mknoloc s) argnames)
            in

            let app_args = List.map2 (fun argname arg ->
              match arg.ptyp_desc with
              | _ when are_the_same arg root_type ->
                  [%expr GT.make self [%e Exp.ident argname] tpo]
              | Ptyp_var v -> [%expr GT.make [%e Exp.ident @@ "f"^v]
                  [%e Exp.ident argname] tpo]
              | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) ->
                  [%expr [%e Exp.ident argname]]
              | Ptyp_constr ({txt=(Lident "int"); _},[]) ->
                  [%expr [%e Exp.ident argname]]
              | Ptyp_constr _ ->
                 [%expr [%e Exp.ident argname]]
                 (* [%expr GT.make [%e Exp.ident @@ lid "self"] [%e Exp.ident @@ lid argname] tpo] *)
              | _ -> raise_errorf "Some cases are not supported when generating application in gcata"
            ) argnames pcd_args
            in

            Exp.case (Pat.construct (Located.lident ~loc name') args_tuple) @@
            Exp.(apply (send (ident "trans") ("c_"^name'))
                 @@ List.map (fun x -> (Nolabel,x))
                   ([ [%expr inh]; [%expr (GT.make self subj tpo)] ] @ app_args)
                )
          )
        in
        [%expr
          let rec [%p (Pat.var @@ mknoloc typename_gcata) ] =
            [%e make_params_lambda_fa ~root_type
              [%expr
              fun trans inh subj ->
                let rec self = [%e make_params_app_fa (Exp.ident typename_gcata)
                    [Exp.ident "trans"] ]
                and tpo = [%e tpo ] in
                [%e match_body]
            ]]
          in
          { GT.gcata = [%e Exp.ident typename_gcata]; GT.plugins = () }
        ]
      in

      let (tt_methods, t_methods, _) = generate_some_methods root_type constrs ~typename in
      let class_t =
        let poly_names = map_type_param_names (fun x -> x) root_type.ptype_params in
        let t_class_params =
          let xs = List.map poly_names ~f:(fun n ->
            Typ.([var n; var @@ "i"^n; var @@ "s"^n; var @@ "gt_a_for_"^n])
            )
          in
          let xs = [ [%type: 'inh]; [%type: 'syn]; [%type: 'tpoT] ] @ (List.flatten xs) in
          invariantize xs
        in
        let params =
          let ps =
            let itself = using_type ~typename root_type in
            MakeMeta.standart_type_prefix ~itself ~gt_a_for_self:itself ()
          in
          (* TODO: hack here. *)
          let poly_params = List.filter_map root_type.ptype_params ~f:(fun (p,_) ->
            match p.ptyp_desc with
            | Ptyp_var name -> Some name
            | _ -> None
            )
          in
          let tail = List.map poly_params ~f:(fun name -> Typ.var @@ sprintf "gt_a_for_%s" name) in
          invariantize @@ ps @ tail
          (* match MakeMeta.t_params ~params:root_type.ptype_params with
          | []
          | [_] -> assert false
          | a::b::xs -> a :: (using_type ~typename root_type, Invariant) :: xs *)
        in
        let params = List.map ~f:fst params in
        Str.single_class ~loc
          ~params:t_class_params
          ~name:typename_t
          ~pat:(Pat.var @@ mknoloc "this")
          [Cf.inherit_ (Cl.constr
                          (Located.lident ~loc typename_meta_t) params)]
      in
      let ans =
        [ MakeMeta.str_meta_gcata_for_algebraic ~root_type ~typename
            ~gcata_name:typename_meta_gcata make_params_app_fa constrs
        ; make_gcata ~root_type ~name:typename_gcata ~metaname:typename_meta_gcata
        ; MakeMeta.str_tt_for_algebraic ~params:root_type.ptype_params ~tt_name:typename_meta_tt constrs
        (* ; make_tt_class_type ~root_type ~typename ~typename_meta_tt ~typename_tt tt_methods *)
        ; MakeMeta.str_class_for_algebraic ~loc
            ~params:root_type.ptype_params ~t_name:typename_meta_t
            ~parent_name:typename_meta_tt
        ; class_t
        ]
      in
      let ans = ans @ show_decls @ gmap_decls in
      ans @ [derivers_bunch]

  | _ -> raise_errorf ~loc:root_type.ptype_loc "%s: some error2" deriver
         *)

let make_interface_class ~loc root_type =
  let params = List.map ~f:fst root_type.ptype_params in
  (* let sort = root_type *)
  let name = root_type.ptype_name in
  (* let manifest = root_type.ptype_manifest in *)

  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (if polyvar)
  *)
  let prepare_params is_poly =
    if is_poly
    then failwith "polyvariants not yet work"
    else prepare_param_triples ~loc params
  in

  let ans ?(is_poly=false) fields =
    Str.class_single ~loc ~name:(sprintf "class_%s" name.txt) fields
      ~params:(prepare_params is_poly |> invariantize)
  in
  visit_typedecl ~loc root_type
    ~onvariant:(fun cds ->
      ans @@
      List.map cds ~f:(fun cd ->
        let methname = sprintf "c_%s" cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "record constructors"
        | Pcstr_tuple ts ->
            Cf.method_ ~loc methname ~flg:Public @@ Cfk_virtual
               (List.fold_right ts ~init:[%type: 'syn] ~f:(Typ.arrow Nolabel)
                |> (Typ.arrow Nolabel [%type: 'inh])
               )
      )
    )
    ~onmanifest:(fun typ ->
          let wrap name params =
            let inh_params =
                List.concat_map params ~f:(fun typ ->
                  [ typ
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("i"^n) )
                  ; map_core_type typ ~onvar:(fun n -> Typ.var ("s"^n) )
                  ]
                )
              in
              let inh_params = inh_params @ inh_syn_ts ~loc () in
              ans
                [ Cf.inherit_ ~loc @@
                  Cl.constr (mknoloc @@ map_longident ~f:(sprintf "class_%s") name)
                    inh_params
                ]
          in

          let rec helper typ = match typ with
          | {ptyp_desc=Ptyp_var name} -> not_implemented "antiphantom types"
          | [%type: string]
          | [%type: char]
          | [%type: int]  ->
              not_implemented "%s " Caml.__FILE__ (* Caml.__LINE__ *)
          | {ptyp_desc=Ptyp_constr ({txt;loc}, params)} ->
              (* a type alias on toplevel *)
              wrap txt params
          | {ptyp_desc=Ptyp_alias (typ, new_name)} ->
              map_core_type typ ~onvar:(fun as_ ->
                if String.equal as_ new_name
                then Typ.constr (Located.lident ~loc name.txt) params
                else Typ.var ~loc as_
              ) |> helper
          | {ptyp_desc=Ptyp_variant (rows,_,_labels)} ->
              (* rows go to virtual methods. label goes to inherit fields *)
              let meths =
                List.map rows ~f:(function
                | Rinherit _ -> assert false
                | Rtag (lab,_,_,args)  ->
                    let methname = sprintf "c_%s" lab in
                    Cf.method_ ~loc methname @@ Cfk_virtual
                      (List.fold_right args ~init:[%type: 'syn]
                         ~f:(Typ.arrow Nolabel)
                       |> (Typ.arrow Nolabel [%type: 'inh])
                      )
                )
              in
              (* TODO: implement inherit fields*)
              ans meths
          | _ -> failwith "not implemented"
          in
          helper typ
    )


let make_gcata ~loc root_type =
  let gcata_pat =
    Pat.var (mknoloc @@
             sprintf "gcata_%s" root_type.ptype_name.txt)
  in
  visit_typedecl ~loc root_type
    ~onvariant:(fun cds ->
      let ans k =
        Str.value ~loc Nonrecursive
          [value_binding ~loc
             ~pat:gcata_pat
             ~expr:[%expr fun tr inh t -> [%e k] ]
          ]
      in
      ans @@ prepare_patt_match ~loc [%expr t] (`Algebraic cds) (fun cd names ->
          List.fold_left ("inh"::names)
            ~init:(Exp.send ~loc [%expr tr] ("c_" ^ cd.pcd_name.txt))
            ~f:(fun acc arg -> Exp.apply ~loc acc [Nolabel, Exp.ident arg])
        )
      )
    ~onmanifest:(fun typ ->
      let rec do_typ t = match t.ptyp_desc with
      | Ptyp_alias (t,_) -> do_typ t
      | Ptyp_constr ({txt},_) ->
          Str.value ~loc Nonrecursive
            [value_binding ~loc
               ~pat:gcata_pat
               ~expr:(Exp.ident_of_long ~loc @@ mknoloc @@
                      map_longident txt ~f:(fun s -> "gcata_"^s) )
            ]
      | Ptyp_variant (rows,_,maybe_labels) ->
          (* [%stri let _gcata_poly = 1] *)
          let ans k =
            Str.value ~loc Nonrecursive
              [value_binding ~loc
                 ~pat:gcata_pat
                 ~expr:[%expr fun tr inh t -> [%e k] ]
              ]
          in
          ans @@ prepare_patt_match_poly ~loc [%expr t] rows maybe_labels
            ~onrow:(fun cname  names ->
              List.fold_left ("inh"::(List.map ~f:fst names))
                ~init:(Exp.send ~loc [%expr tr] ("c_" ^ cname))
                ~f:(fun acc arg -> 
                       Exp.apply ~loc acc [Nolabel, Exp.ident arg]
                   )
            )
            ~onlabel:1
        
      in
      do_typ typ
    )

let do_typ ~loc options is_rec root_type =
  let intf_class = make_interface_class ~loc root_type in
  let gcata = make_gcata ~loc root_type in
  intf_class :: gcata ::
  (if options.gt_show
  then Show.do_single ~loc ~is_rec:true root_type
  else [])


let do_mutal_types ~loc options tdecls =
  List.concat_map tdecls ~f:(fun tdecl ->
    let intf_class = make_interface_class ~loc tdecl in
    let gcata = make_gcata ~loc tdecl in
    [intf_class; gcata]
  ) @

  (if options.gt_show
  then Show.do_mutals ~loc ~is_rec:true tdecls
  else [])

let str_type_decl ~loc ~path (rec_flag, tdls) gt_show gt_gmap =
  let options = {gt_show; gt_gmap} in
  match rec_flag, tdls with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ ~loc options true tdecl
  | Recursive, ts      -> do_mutal_types ~loc options ts
  | Nonrecursive, ts ->
      List.concat_map ~f:(do_typ ~loc options false) tdls
