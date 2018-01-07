(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let expr_of_arg (reprname: string) typ root_type =
  let loc = root_type.ptype_loc in
  let open Ppx_core.Location in
  let rec helper ?(loc=Location.none) ?(toplevel=false) =
    let maybe_apply e : Ppx_core.expression =
      if toplevel then [%expr [%e e] [%e Exp.ident ~loc reprname ] ]
      else e
    in
    function
    | x when are_the_same x root_type ->
     if toplevel
     then [%expr GT.([%e pexp_field ~loc (Exp.ident ~loc reprname)
                        (Located.lident ~loc "fx") ]) () ]
     else [%expr GT.transform
         [%e Exp.ident ~loc root_type.ptype_name.txt]
         subj.GT.t#a this () ]
    | {ptyp_desc=Ptyp_var _alpha; _} -> Exp.send ~loc [%expr subj.GT.t] _alpha
    | [%type: int]
    | [%type: GT.int] ->
        maybe_apply [%expr GT.lift GT.int.GT.plugins#show () ]
    | [%type: string]
    | [%type: GT.string] ->
     maybe_apply [%expr GT.transform GT.string (new GT.show_string_t) () ]
    | [%type: [%t? t] GT.list]
    | [%type: [%t? t] list] ->
     maybe_apply [%expr GT.lift (GT.list.GT.plugins#show [%e helper t]) () ]
    | {ptyp_desc=Ptyp_constr ({txt=Lident cname;_},
                             [{ptyp_desc=Ptyp_constr({txt=Lident argname;_},
                                                     _)
                              }]); _ }
     when String.equal argname root_type.ptype_name.txt ->
       let head = List.fold_left
           ~f:(fun acc (tparam,_) ->
              match tparam with
              | {ptyp_desc=Ptyp_var alpha; _} ->
                  [%expr [%e acc] [%e pexp_send ~loc [%expr subj.GT.t] alpha ] ]
              | _ -> assert false
           )
           ~init:[%expr GT.transform [%e
               pexp_ident ~loc @@Located.lident ~loc argname ]]
           root_type.ptype_params
       in
       maybe_apply
         [%expr  GT.transform
                 [%e pexp_ident ~loc @@ Located.lident ~loc cname]
                 ([%e head] this)
                 [%e pexp_new ~loc @@ Located.lident ~loc @@
                   sprintf "show_%s_t" cname ]
                 ()
         ]
    | {ptyp_desc=Ptyp_constr ({txt=Lident cname;_},
                             [typ_arg1]); }
     ->
     maybe_apply
       [%expr GT.transform
           [%e Exp.ident ~loc cname]
           [%e helper typ_arg1 ]
           [%e Exp.new_ ~loc @@ Located.lident ~loc (sprintf "show_%s_t" cname) ]
           ()
       ]
    | _ ->
        Exp.field ~loc (Exp.ident ~loc reprname) (Located.lident ~loc "GT.fx") 
  in

  match typ with
  | {ptyp_desc=Ptyp_var alpha; _} ->
    pexp_apply ~loc (pexp_ident ~loc @@ Located.lident ~loc ("for_"^alpha) )
      [Nolabel, Exp.ident ~loc reprname]
    (* [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ] () ] *)
  | _ -> helper ~loc ~toplevel:true typ


let plugin_name = "show"

(*
let extra_params _ = []

let inh  ?(loc=Location.none) _ = [%type: unit]
let synh ?(loc=Location.none) _ = [%type: string]

let synh_root tdecl _ =
  let loc = tdecl.ptype_loc in
  [%type: string]

let core = function
  | [%type: int] as t ->
    let loc = t.ptyp_loc in
    pcl_structure ~loc (class_structure ~self:(ppat_any ~loc)
                          ~fields:[ pcf_inherit ~loc Fresh
                              (pcl_constr ~loc (Located.lident ~loc "GT.show_int_t") [])
                              None ])
  | t ->
    let b = Buffer.create 40 in
    let fmt = Caml.Format.formatter_of_buffer b in
    Pprintast.core_type fmt (Caml.Obj.magic t);
    Caml.Format.pp_flush_formatter fmt;
    raise_errorf "%s\n%s" "not implemented?4 " (Buffer.contents b)

let is_ground_enough = function
  | [%type: string] -> true
  | [%type: char] -> true
  | [%type: int] -> true
  | _ -> false

let process_ground_enough ~n (topTr, specTr, appTr, types, inhTypes) t =
  let loc = t.ptyp_loc in
  let wrap orig ident =
    let transformer_pat = Pat.var @@ mknoloc @@ sprintf "for_%d" n in
    let for_expr = [%expr let open GT in
      lift [%e pexp_field ~loc ident
          (Located.lident ~loc "plugins") ] #show () ]
    in
    ( topTr
    , (fun e -> Cl.let_ Nonrecursive [Vb.mk transformer_pat for_expr] (specTr e))
    , (pexp_ident ~loc @@  Located.lident ~loc (sprintf "for_%d" n)) :: appTr
    , types
    , orig::orig::inhTypes)
  in
  match t with
  | [%type: string] -> wrap t (pexp_ident ~loc @@ Located.lident ~loc "string")
  | [%type: char]   -> wrap t (pexp_ident ~loc @@ Located.lident ~loc "char")
  | [%type: int]    -> wrap t (pexp_ident ~loc @@ Located.lident ~loc "int")
  | typ -> failwith (sprintf "%s %d: not ground enough `%s`" Caml.__FILE__ Caml.__LINE__
                            (string_of_core_type typ))

let meta_for_alias ~name ~root_type ~manifest : structure_item =
  (* params of meta class depend only on type parameters of a type being processed *)
  let loc = root_type.ptype_loc in
  let eval_params ps =
    let rec helper ~n ~acc:(topTr, specTr, appTr, types, inhTypes) = function
    | [] -> (topTr, specTr, appTr, types, inhTypes)
    | {ptyp_desc=Ptyp_var name; _} :: tl ->
        let new_params = [Typ.var name; Typ.var @@ name^"_holder"] in
        let transformer_pat = pvar ~loc @@ sprintf "for_%d_%s" n name in
        let transformer_expr = pexp_ident ~loc @@ Located.lident ~loc (sprintf "for_%d_%s" n name) in
        helper ~n:(n-1) tl
                  ~acc: ( (fun e -> Cl.fun_ Nolabel None transformer_pat (topTr e))
                        , specTr
                        , transformer_expr :: appTr
                        , new_params::types
                        , new_params@inhTypes)

    | orig :: tl  when are_the_same orig root_type ->
        (* let transformer_pat  = Pat.var @@ mknoloc @@ sprintf "for_%d_%s" n name in *)
        (* let transformer_expr = Exp.ident @@ lid @@ sprintf "for_me" in *)
        let new_inh = [ using_type ~typename:root_type.ptype_name.txt root_type; [%type: 'self_holder] ] in
        helper  ~n:(n-1) tl
                ~acc: ( topTr
                      , specTr
                      , [%expr for_me] :: appTr
                      , types
                      , new_inh@inhTypes)
    | orig :: tl when is_ground_enough orig ->
        helper ~n:(n-1) tl ~acc:(process_ground_enough ~n (topTr, specTr, appTr, types, inhTypes) orig)
    (* | ([%type: string] as orig) :: tl ->
        let transformer_pat = Pat.var @@ mknoloc @@ sprintf "for_%d" n in
        let for_expr = [%expr GT.lift (GT.string.GT.plugins)#show () ] in
        helper ~n:(n-1) tl
                  ~acc: ( topTr
                        , (fun e -> Cl.let_ Nonrecursive [Vb.mk transformer_pat for_expr] (specTr e))
                        , (Exp.ident @@ lid @@ sprintf "for_%d" n) :: appTr
                        , types
                        , orig::orig::inhTypes) *)
    | typ :: _ -> failwith (sprintf "%s %d: Don't know what to do about the type `%s`" Caml.__FILE__ Caml.__LINE__
                              (string_of_core_type typ))
    in
    let topTr, specTr, appTr, types, inhTypes =
      (* we reverse inout to make a result in th eright order *)
      helper ~n:(List.length ps) ~acc:((fun x -> x),(fun x -> x),[ [%expr for_me] ],[],[]) (List.rev ps) in

    (* now we add for_me *)
    let topTr expr = topTr (pcl_fun ~loc Nolabel None (for_me_patt ~loc ()) expr) in
    let types = [%type: 'tpoT] :: (List.concat types) in
    let types = types @ [ [%type: 'self_holder] ] in

    let inhTypes =
      let last_inh_self_holder =
        ptyp_constr ~loc (Located.lident ~loc root_type.ptype_name.txt) @@
          map_type_param_names root_type.ptype_params ~f:(fun name -> ptyp_var ~loc @@ name^"_holder" )
      in
      [ [%type: 'tpoT] ] @ inhTypes @ [ last_inh_self_holder ]
    in
    let appTr = nolabelize appTr in
    (topTr, specTr, appTr, types, inhTypes)
  in
  match manifest.ptyp_desc with
  | Ptyp_constr ({txt=ident;_}, params) ->
    let topTr, specTr, appTr, types, inhTypes = eval_params params in
    (* let clas =
      Cl.structure (Cstr.mk (Pat.any ()) )
    in
    let clas = specTr clas in *)
    let parent_meta = affect_longident ident ~f:(sprintf "show_meta_%s") in
    Str.single_class ~params:(invariantize types) ~name
      ~wrap:(fun x -> topTr @@ specTr x)
      [
        Cf.inherit_ (Cl.apply (Cl.constr (mknoloc parent_meta) inhTypes) appTr)
      ]
  | _ -> failwith "not implemented"

(* make normal class like show_list *)
let for_alias ~name ~root_type ~manifest : structure_item =
  let loc = root_type.ptype_loc in
  match manifest.ptyp_desc with
  | Ptyp_constr ({txt=ident;_}, params) ->
      let appTr = map_type_param_names root_type.ptype_params
        ~f:(fun name ->
              let name = "p" ^ name in
              [%expr fun [%p Pat.var@@ mknoloc name  ] ->
                [%e Exp.ident ~loc name].GT.fx ()])
      in
      let appTr = nolabelize @@ (appTr @ [ for_me_expr ~loc () ] ) in
      let inh_params =
        map_type_param_names root_type.ptype_params
          ~f:(fun name ->
                let itself = Typ.var name in
                [ itself
                ; make_gt_a_typ ~inh:[%type: unit] ~itself ~syn:[%type: string] ()
                ])
        |> List.concat
        |> (fun xs -> (Typ.alias (params_obj ~inh ~syn:synh root_type) "tpoT") :: xs @ [ [%type: 'self_holder] ]   )
      in
      let typname = root_type.ptype_name.txt in
      let class_params = root_type.ptype_params @
        [ ([%type: 'self_holder], Invariant) ]
      in
      Str.single_class ~params:class_params ~name ~pat:[%pat? self] ~virt:Concrete
        ~wrap:(Cl.fun_ Nolabel None (for_me_patt ~loc ()))
        [
          Cf.inherit_ 
              (Cl.apply (Cl.constr (Located.lident ~loc @@ sprintf "show_meta_%s" typname) inh_params) appTr) 
        (* ; let e =
            let patts,exprs =
              map_type_param_names root_type.ptype_params
                ~f:(fun name ->
                    let name = sprintf "transform_%s" name in
                    (Pat.var @@ mknoloc name , Exp.ident @@ lid name))
              |> List.split
            in
            let exprs = nolabelize @@ ( exprs @ [[%expr self]]) in
            let patts = patts @ [ Pat.var @@ mknoloc "for_me" ] in
            Exp.fun_list ~args:patts @@
              Exp.apply (Exp.ident @@ lid @@  sprintf "%s_gcata" typname) exprs
          in
          Cf.method_ (mknoloc @@ sprintf "t_%s" typname) Public (Cfk_concrete (Fresh, e) ) *)
        ]
  | _ -> assert false

let constructor root_type constr =
  let loc = root_type.ptype_loc in
  let name = constr.pcd_name in
  (* let param_names = map_type_param_names ~f:(fun x -> x) root_type.ptype_params in *)
  match constr.pcd_args with
  | Pcstr_tuple arg_types ->
    let arg_names = List.mapi (fun n _ -> sprintf "p%d" n) arg_types in
    let body =
      match List.combine arg_names arg_types with
      | [] -> Exp.constant (Pconst_string (name.txt ^ " ()", None))
      | [(argn,argt)] -> [%expr
                            [%e Exp.constant (Pconst_string (name.txt ^ " (", None)) ] ^
                            [%e expr_of_arg argn argt root_type] ^ ")"
                         ]
      | args ->
         let xs = List.map (fun (argn,argt) -> expr_of_arg argn argt root_type) args in
         [%expr
             [%e Exp.constant (Pconst_string (name.txt ^ " (", None)) ] ^
             (String.concat ", " [%e Exp.make_list xs ] ^ ")")
         ]
    in
    let e =
      let pats =
        [ Pat.var @@ mknoloc "inh"; Pat.var @@ mknoloc "subj" ] @
        (List.map2 arg_names arg_types ~f:(fun name t ->
          let always = Pat.(var @@ mknoloc name) in
          match t.ptyp_desc with
          | Ptyp_var name -> Pat.(constraint_ always (Typ.var @@ name^"_holder") )
          | _ -> always
        ) )
      in
      List.fold_right pats ~f:(fun pat acc -> Exp.fun_ Nolabel None pat acc) ~init:body
    in
    Cf.method_ ("c_" ^ name.txt) Public (Cfk_concrete (Fresh, e))
  | _ -> failwith "Non-tuple constructor arguments are not supported"
         *)

let make_new_names n =
  List.init n ~f:(fun n ->  Char.to_string @@ Char.of_int_exn (n + Char.to_int 'a'))
            

let default_inh = let loc = Location.none in [%type: unit]
let default_syn = let loc = Location.none in [%type: string]

let make_class ~loc tdecl ~is_rec mutal_names =
  let cur_name = tdecl.ptype_name.txt in
  (* TODO: support is_rec to handle `type nonrec t = option t` *)
  let ans fields =
    (* inherit class_t and prepare to put other members *)
    Str.class_single ~loc
      ~virt:Concrete
      ~params:tdecl.ptype_params
      ~wrap:(fun body ->
        (* constructor arguments are *)
        let names = List.map mutal_names ~f:(Pat.sprintf ~loc "%s_%s" plugin_name) @
                    [Pat.(alias ~loc (sprintf "show_%s" cur_name) "_fself")] @
                    List.map tdecl.ptype_params ~f:(fun (t,_) ->
                      match t.ptyp_desc with
                      | Ptyp_var name -> Pat.sprintf ~loc "f%s" name
                      | _ -> assert false
                    )
        in
        Cl.fun_list names body
      )
      ~name:(sprintf "%s_%s_stub" plugin_name cur_name)
      @@
      [ let inh_params = prepare_param_triples ~loc
            ~inh:(fun ~loc _ -> default_inh)
            ~syn:(fun ~loc _ -> default_syn)
            (List.map ~f:fst tdecl.ptype_params)
        in
        Cf.inherit_ (Cl.constr (Located.lident ~loc ("class_"^cur_name)) inh_params)
      ] @ fields
  in

  let is_self_rec t = is_rec &&
    (match t.ptyp_desc with
    | Ptyp_var _ -> false
    | Ptyp_constr ({txt=Lident s}, params)
      when String.equal s cur_name && List.length params = List.length tdecl.ptype_params &&
           List.for_all2_exn params tdecl.ptype_params ~f:(fun a (b,_) -> 0=compare_core_type a b)
      -> is_rec
    | _ -> false
    )
  in
  ans @@ visit_typedecl ~loc tdecl
    ~onvariant:(fun cds ->
      List.map cds ~f:(fun cd ->
        let constr_name = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "wtf"
        | Pcstr_tuple ts ->
            let names = List.mapi ts
                ~f:(fun n _ -> Char.to_string @@ Char.of_int_exn
                       (n + Char.to_int 'a'))
            in
            Cf.method_concrete ~loc ("c_"^cd.pcd_name.txt)
              [%expr fun () -> [%e
                Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
                if List.length ts = 0
                then Exp.constant ~loc (Pconst_string (constr_name, None))
                else
                  let rec do_typ ?with_arg t =
                    let app_arg e =
                      match with_arg with
                      | Some x -> Exp.apply ~loc e [Nolabel, Exp.ident x]
                      | None -> e
                    in
                    if is_self_rec t then app_arg [%expr _fself () ]
                    else
                      match t with
                      | [%type: int] -> app_arg [%expr string_of_int ]
                      | {ptyp_desc=Ptyp_var name} ->
                          app_arg Exp.(sprintf "f%s" name)
                      | {ptyp_desc=Ptyp_tuple params} ->
                          app_arg @@
                          Exp.apply
                            (Exp.sprintf "show_tuple%d" (List.length params))
                            (List.map params ~f:(fun t -> Nolabel, do_typ t))

                      | {ptyp_desc=Ptyp_constr ({txt}, params) } ->
                          app_arg @@
                          Exp.apply
                            (Exp.ident_of_long @@ mknoloc @@
                             map_longident ~f:((^)"show_") txt)
                            (List.map params ~f:(fun t -> Nolabel, do_typ t))
                      | _ -> failwith "Finish it!"
                  in
                  List.fold_left
                    (List.zip_exn names ts)
                    ~f:(fun acc (name, typ) ->
                      Exp.apply ~loc acc [(Nolabel, do_typ ~with_arg:name typ)] )
                    ~init:[%expr Format.sprintf [%e
                        let fmt = String.concat ~sep:", " @@ List.map names
                            ~f:(fun _ -> "%s")
                        in
                        Exp.constant ~loc @@  const_string @@
                        sprintf "%s(%s)" constr_name fmt
                      ]]
              ]]
      )
    )


let make_trans_functions ~loc ~is_rec mutal_names tdecls =
  (* we will generate mutally recrsive showers here *)

  let make_class_name typname = sprintf "show_%s_stub" typname in
  Str.value ~loc Recursive @@ List.map tdecls ~f:(fun tdecl ->
    let cur_name = tdecl.ptype_name.txt in
    let others =
      List.filter mutal_names ~f:(String.(<>) cur_name)
    in
    value_binding ~loc ~pat:(Pat.sprintf "show_%s" tdecl.ptype_name.txt)
      ~expr:(
        let arg_transfrs = map_type_param_names tdecl.ptype_params ~f:((^)"f") in
        Exp.fun_list ~loc
          ~args:(List.map arg_transfrs ~f:(Pat.sprintf ~loc "%s"))
          [%expr fun t -> fix0 (fun self ->
            [%e Exp.apply1 ~loc (Exp.sprintf ~loc "gcata_%s" cur_name) @@
              Exp.apply ~loc (Exp.new_ ~loc @@ Located.lident ~loc @@
                              make_class_name cur_name) @@
              (List.map others ~f:(fun name -> Nolabel,[%expr [%e Exp.sprintf ~loc "show_%s" name]])
               @ [Nolabel, [%expr self] ]
               @ List.map arg_transfrs ~f:(fun s -> Nolabel, [%expr fun () -> [%e Exp.sprintf ~loc "%s" s]])
              )
            ]
          ) t
          ]
      )
  )

let make_shortend_class ~loc ~is_rec mutal_names tdecls =
  List.map tdecls ~f:(fun tdecl ->
    let mutal_names = List.filter mutal_names ~f:(String.(<>) tdecl.ptype_name.txt) in
    let class_name = sprintf "show_%s" tdecl.ptype_name.txt in
    let stub_name = class_name ^ "_stub" in
    let mut_funcs = List.map ~f:((^)"show_") mutal_names in
    let real_args = "fself" :: (List.map ~f:((^)"f") @@ make_new_names (List.length tdecl.ptype_params)) in
    Str.single_class ~loc ~name:class_name
      ~wrap:(Cl.fun_list @@ List.map ~f:(Pat.sprintf ~loc "%s") @@ real_args)
      ~params:tdecl.ptype_params
      [ Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr ~loc (Located.lident ~loc stub_name) (List.map ~f:fst tdecl.ptype_params))
          (List.map ~f:(fun s -> Nolabel, Exp.sprintf ~loc "%s" s) real_args)
      ]
  )

let do_mutals ~loc ~is_rec tdecls =
  (* for mutal recursion we need to generate two classes and one function *)
  let mut_names = List.map tdecls ~f:(fun td -> td.ptype_name.txt) in
  List.concat_map tdecls ~f:(fun tdecl ->
    [ make_class ~loc ~is_rec:true tdecl @@
      List.filter mut_names ~f:(String.(<>) tdecl.ptype_name.txt)
    ]
  ) @
  [make_trans_functions ~loc ~is_rec:true mut_names tdecls] @
  (make_shortend_class  ~loc ~is_rec:true mut_names tdecls)
