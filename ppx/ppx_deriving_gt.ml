(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Printf
open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving

module List = struct
  include ListLabels
  let split3 xs =
    List.fold_right (fun (a,b,c) (ac,bc,cc) -> (a::ac,b::bc,c::cc))
      xs ([],[],[])
  let filter_map ~f xs =
    List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []
  let last_exn xs = List.hd @@ List.rev xs
end

let deriver = "gt"
let raise_errorf = Ppx_deriving.raise_errorf

type supported_derivers = { gt_show: bool; gt_eq: bool; gt_gmap: bool }


let parse_options options =
  List.fold_left ~f:(fun acc (name,expr) ->
    match name with
    | "show" -> {acc with gt_show = true}
    | "gmap" -> {acc with gt_gmap = true}
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)
    ~init:{ gt_show=false; gt_eq=false; gt_gmap=false }
    options

let argn = Printf.sprintf "a%d"
(*
let pp_type_of_decl ~options ~path type_decl =
  let opts = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: Format.formatter -> [%t typ] -> Ppx_deriving_runtime.unit]

let show_type_of_decl ~options ~path type_decl =
  let opts  = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: [%t typ] -> Ppx_deriving_runtime.string]

let sig_of_type ~options ~path type_decl =
  let opts = parse_options options in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
              (pp_type_of_decl ~options ~path type_decl));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl))
              (show_type_of_decl ~options ~path type_decl))]

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  match attr_printer typ.ptyp_attributes with
  | Some printer ->
    let printer =
      [%expr (let fprintf = Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]
    in
    [%expr [%e Ppx_deriving.quote quoter printer] fmt]
  | None ->
  if attr_opaque typ.ptyp_attributes then
    [%expr fun _ -> Format.pp_print_string fmt "<opaque>"]
  else
    let format x = [%expr Format.fprintf fmt [%e str x]] in
    let seq start finish fold typ =
      [%expr fun x ->
        Format.fprintf fmt [%e str start];
        ignore ([%e fold] (fun sep x ->
          if sep then Format.fprintf fmt ";@ ";
          [%e expr_of_typ typ] x; true) false x);
        Format.fprintf fmt [%e str finish];]
    in
    match typ with
    | [%type: _] -> [%expr fun _ -> Format.pp_print_string fmt "_"]
    | { ptyp_desc = Ptyp_arrow _ } ->
      [%expr fun _ -> Format.pp_print_string fmt "<fun>"]
    | { ptyp_desc = Ptyp_constr _ } ->
      let builtin = not (attr_nobuiltin typ.ptyp_attributes) in

      begin match builtin, typ with
      | true, [%type: unit]        -> [%expr fun () -> Format.pp_print_string fmt "()"]
      | true, [%type: int]         -> format "%d"
      | true, [%type: int32]
      | true, [%type: Int32.t]     -> format "%ldl"
      | true, [%type: int64]
      | true, [%type: Int64.t]     -> format "%LdL"
      | true, [%type: nativeint]
      | true, [%type: Nativeint.t] -> format "%ndn"
      | true, [%type: float]       -> format "%F"
      | true, [%type: bool]        -> format "%B"
      | true, [%type: char]        -> format "%C"
      | true, [%type: string]
      | true, [%type: String.t]    -> format "%S"
      | true, [%type: bytes]
      | true, [%type: Bytes.t] ->
        [%expr fun x -> Format.fprintf fmt "%S" (Bytes.to_string x)]
      | true, [%type: [%t? typ] ref] ->
        [%expr fun x ->
          Format.pp_print_string fmt "ref (";
          [%e expr_of_typ typ] !x;
          Format.pp_print_string fmt ")"]
      | true, [%type: [%t? typ] list]  -> seq "[@[<hov>"   "@]]" [%expr List.fold_left]  typ
      | true, [%type: [%t? typ] array] -> seq "[|@[<hov>" "@]|]" [%expr Array.fold_left] typ
      | true, [%type: [%t? typ] option] ->
        [%expr
          function
          | None -> Format.pp_print_string fmt "None"
          | Some x ->
            Format.pp_print_string fmt "(Some ";
            [%e expr_of_typ typ] x;
            Format.pp_print_string fmt ")"]
      | true, ([%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t]) ->
        [%expr fun x ->
          if Lazy.is_val x then [%e expr_of_typ typ] (Lazy.force x)
          else Format.pp_print_string fmt "<not evaluated>"]
      | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let args_pp = List.map (fun typ -> [%expr fun fmt -> [%e expr_of_typ typ]]) args in
        let printer =
          match attr_polyprinter typ.ptyp_attributes with
          | Some printer ->
            [%expr (let fprintf = Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]
          | None ->
            Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "pp") lid))
        in
        app (Ppx_deriving.quote quoter printer)
            (args_pp @ [[%expr fmt]])
      | _ -> assert false
      end
    | { ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs in
      [%expr
        fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
        Format.fprintf fmt "(@[<hov>";
        [%e args |> Ppx_deriving.(fold_exprs
                (seq_reduce ~sep:[%expr Format.fprintf fmt ",@ "]))];
        Format.fprintf fmt "@])"]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (Pat.variant label None)
                     [%expr Format.pp_print_string fmt [%e str ("`" ^ label)]]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (Pat.variant label (Some [%pat? x]))
                     [%expr Format.fprintf fmt [%e str ("`" ^ label ^ " (@[<hov>")];
                            [%e expr_of_typ typ] x;
                            Format.fprintf fmt "@])"]
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case [%pat? [%p Pat.type_ tname] as x]
                     [%expr [%e expr_of_typ typ] x]
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      Exp.function_ cases
    | { ptyp_desc = Ptyp_var name } -> [%expr [%e evar ("poly_"^name)] fmt]
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)
 *)

(* open Parsetree *)

module Exp = struct
  include Exp

  let make_pair a b = tuple [a;b]
  let make_list xs =
    List.fold_right xs
      ~f:(fun e acc -> construct (lid "::") (Some (make_pair e acc)) )
      ~init:(Exp.construct (lid "[]") None)

end

let default_params root_type =
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
  List.map (fun x -> (x,Invariant) ) ps

let using_type ~typename root_type =
  (* generation type specification by type declaration *)
  Typ.constr (lid typename) (List.map ~f:fst @@ root_type.ptype_params)

let arr_of_param t =
  (* does from 'a the 'ia -> 'a -> 'sa *)
  let open Typ in
  match t.ptyp_desc with
  | Ptyp_var n ->
      (mknoloc n, [],
        [%type: [%t var @@ "i"^n] -> [%t var n] -> [%t var @@ "s"^n]] )
  | _ ->
      raise_errorf "arr_of_param: not all type params are supported" deriver


let params_obj root_type =
  (* converts 'a, 'b to
     < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
   *)
  let f (t,_) = arr_of_param t in
  Typ.object_ (List.map f root_type.ptype_params) Closed

(* Used when we need to check that type we working on references himself in
  it's body *)
let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (* Pprintast.core_type Format.std_formatter (Obj.magic typ);
  Format.pp_force_newline Format.std_formatter ();
  Format.pp_print_flush Format.std_formatter (); *)

  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Lident xxx},_) ->
    let b = (xxx = tdecl.ptype_name.txt) in
    (* printf "xxx = %s, tdecl.ptype_name.txt = %s, %b\n%!" xxx tdecl.ptype_name.txt b; *)
    b
  | _ ->
    false
  )

let make_params_lambda_generic ~root_type namer expr  =
  List.fold_right ~f:(fun ({ptyp_desc},_) acc ->
    match ptyp_desc with
    | Ptyp_var name -> [%expr fun [%p pvar @@ namer name] -> [%e acc ] ]
    | _ -> assert false
  ) root_type.ptype_params ~init:expr

let make_params_lambda_a  = make_params_lambda_generic (fun name -> name)
let make_params_lambda_fa = make_params_lambda_generic ((^)"f")

let wrap_with_fa ?(use_lift=false) ~root_type func lasts =
  let right =
    List.map (function ({ptyp_desc; _ },_) ->
      match ptyp_desc with
      | Ptyp_var name ->
         (Nolabel,
          if use_lift then [%expr GT.lift [%e Exp.ident @@ lid ("f"^ name)]]
          else  Exp.ident @@ lid ("f"^ name)
            )
      | _ -> assert false) root_type.ptype_params
  in
  let right = right @ (List.map (fun typ -> (Nolabel, typ)) lasts) in
  let right = Exp.apply func right in
  make_params_lambda_fa ~root_type right

(* There we generate three lists
  1. Method signature for tt class type
  2. Virtual methods for t class structure
  3. Virtual methods for t class signature
  *)
let generate_some_methods ~typename ?(t_virtual=false) root_type constrs =
  let t_typename = "t_" ^ typename in
  let xs = List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
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
    ( Ctf.method_ (mknoloc constr_name) Public Concrete ts,
      (* Cf.method_  (mknoloc constr_name) Public (Cfk_virtual ts)  *)
      Cf.method_  (mknoloc constr_name) Public (Cfk_virtual ts),
      Ctf.method_ (mknoloc constr_name) Public Virtual ts
    )
  ) constrs
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
            [ Ctf.method_ (mknoloc ("t_" ^ typename))  Public Concrete
                meth_main_mapper_typ
            ]
  in

  let main_mapper_body =
    wrap_with_fa ~use_lift:false
      [%expr GT.transform [%e Exp.ident @@ lid typename]]
      [  [%expr this] ]
  in
  let ts = ts @ [ Cf.method_ (mknoloc t_typename) Public
                    (Cfk_concrete (Fresh, main_mapper_body ~root_type))
                ]
  in
  let ts_sigs = ts_sigs @
    [ Ctf.method_ (mknoloc t_typename) Public Concrete
        meth_main_mapper_typ
    ]
  in
  (tts, ts, ts_sigs)

let make_gt_a_typ ?(inh=[%type: 'inh]) ?(itself=[%type: 'type_itself]) ?(syn=[%type: 'syn]) ?(tpoT=[%type: 'tpoT]) () =
  (* TODO: maybe add extra string argument to concat it with type variables to get
              ('a_inh, 'a, 'a_syn, 'heck) GT.a
  *)
  [%type: ([%t inh], [%t itself], [%t syn], [%t tpoT]) GT.a]

module MakeMeta = struct
  let str_tt_for_algebraic ~params ~typename cases_list =
    failwith "not implemented"
  let str_tt_for_algebraic ~params ~typename cases_list =
    let poly_params = List.filter_map params ~f:(fun (p,_) ->
      match p.ptyp_desc with
      | Ptyp_var name -> Some name
      | _ -> None
      )
    in
    if List.length params <> List.length poly_params
    then failwith "consytructor declaration has not vars in a params";

    let params =
      let ps = [ [%type: 'tpoT]; [%type: 'type_itself] ] in
      let ps = ps @
        List.map (fun s -> Typ.var @@ "gt_a_for_"^s) poly_params
      in
      let ps = ps @ [ [%type: 'inh]; [%type: 'syn] ] in
      List.map (fun p -> (p, Invariant)) ps
    in
    let meths = List.map cases_list ~f:(fun ctr ->
      if ctr.pcd_res <> None
      then failwith "GADT not supported";
      match ctr.pcd_args with
      | Pcstr_tuple xs ->
        let xs2 = List.map xs ~f:(fun arg -> match arg.ptyp_desc with
          | Ptyp_var name -> Typ.var (sprintf "gt_a_for_%s" name)
          | Ptyp_constr _ -> arg
          | _ -> assert false
        )
        in
        let methname = {ctr.pcd_name with txt = "c_" ^ ctr.pcd_name.txt} in
        let xs2 = [%type: 'inh] :: (make_gt_a_typ ()) :: xs2 in
        Ctf.method_ methname Public Virtual @@
          List.fold_right xs2 ~init:[%type: 'syn] ~f:(Typ.arrow Nolabel)
      | Pcstr_record _ -> failwith "not supported"
    ) in
    let name = sprintf "%s_meta_tt" typename in
    Str.class_type [Ci.mk ~virt:Virtual ~params
                    (Location.mknoloc name) @@
                  Cty.signature (Csig.mk [%type: _] meths) ]
end

let make_tt_class_type ~root_type ~typename ~typename_meta_tt ~typename_tt tt_methods =
  (* TODO: fix derty hack: there we suppose that last element of tt_methods
    is a transformer for a whole type
  *)

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
                    (Location.mknoloc typename_tt) @@
                  Cty.signature (Csig.mk [%type: _] methods) ]

let make_params_longarrow ~root_type typ =
  List.fold_right ~f:(fun ({ptyp_desc},_) acc ->
    match ptyp_desc with
    | Ptyp_var n ->
       Typ.(arrow Nolabel
                  [%type: [%t var@@ "i"^n] -> [%t var n] -> [%t var @@ "s"^n]]
                  acc)
    | _ -> assert false) root_type.ptype_params ~init:typ

let subclass_obj ~root_type typename_tt =
  (* makes ('a,'ia,'sa,...,'inh,'syn)#typename_tt  *)
  Typ.class_ (lid typename_tt) @@ List.map fst (default_params root_type)

let any_typ = [%type: _]

let gt_repr_typ_wrap ~typename ~root_type arg =
  let typename_tt = typename ^ "_tt" in
  let tail = [%type: 'inh -> [%t using_type ~typename root_type ] -> 'syn ] in
  let subclass = subclass_obj typename_tt ~root_type in
  [%type: ([%t make_params_longarrow ~root_type
              [%type: [%t subclass] -> [%t tail]]],
           [%t arg]) GT.t ]

let inherit_field_gen ~name ~root_type ~inh ~synh ~synh_root wrap =
  let synhs, types = List.split @@ List.map
    (fun ({ptyp_desc; _},_) -> match ptyp_desc with
    | Ptyp_var name -> (synh name, Typ.[var name; inh name; synh name ])
    | _ -> assert false
    ) root_type.ptype_params
  in
  let types = List.concat types in
  wrap Typ.(types @ [ [%type: unit]; synh_root root_type synhs ])

let inherit_cf ~name ~root_type ~inh ~synh ~synh_root =
  inherit_field_gen ~name ~root_type ~inh ~synh ~synh_root
    (fun t -> Cf.inherit_ Fresh (Cl.constr (lid name) t) None)

(* let inherit_ctf ~name ~root_type =
  inherit_field_gen ~name ~root_type (fun t -> Ctf.inherit_ @@ Cty.constr (lid name) t) *)

let sig_of_type ~options ~path ({ ptype_params=type_params } as root_type) =
  let { gt_show; gt_gmap } = parse_options options in

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
                (Csig.mk any_typ t_meth_sigs)
          ]

      ]
    in
    (* let ans = if not gt_show then ans else ans @ (show_decls root_type) in *)
    (* TODO: add gmap here *)

    let derivers_bunch =
      let wrap_meth mname cname =
        let typname = root_type.ptype_name.txt in
        let body =
          wrap_with_fa ~use_lift:true [%expr GT.transform [%e Exp.ident @@ lid typname]] ~root_type
            [ Exp.new_ @@ lid cname; [%expr () ] ]
        in
        Cf.method_ (Location.mknoloc mname) Public (Cfk_concrete (Fresh, body))
      in
      let gcata_part =
        let args2 = List.map (fun (t,_) -> let (_,_,x) = arr_of_param t in x) type_params in
        List.fold_right args2
          ~f:(Typ.arrow Nolabel)
          ~init:[%type: [%t subclass_obj ~root_type typename_tt ] ->
                    'inh -> [%t using_type ~typename root_type ] -> 'syn ]
      in
      let plugins_part =
        let for_show =
          if not gt_show then [] else
          let xs = List.map (fun (typ,_) -> [%type: [%t typ] -> string]) type_params in
          [ (mknoloc "show", [],
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

    val inh  : string -> core_type
    val synh : string -> core_type

    val synh_root : type_declaration -> core_type list -> core_type

    val core : core_type -> class_expr
    val constructor : type_declaration -> constructor_declaration -> class_field
  end

let plugin_decls (module P: Plugin) root_type =
  let type_decl_handler root_type =
    let typename    = root_type.ptype_name.txt in
    let typename_t  = typename ^ "_t"  in
    let plugin_name = P.name ^ "_" ^ typename ^ "_t" in
    match root_type.ptype_kind with
    | Ptype_abstract -> (match root_type.ptype_manifest with
      | Some manifest ->
          [Str.class_ [Ci.mk ~virt:Concrete ~params:[] (mknoloc plugin_name) (P.core manifest) ]]
        (* P.core manifest *)
      | None -> failwith "Not implemented")
    | Ptype_variant constrs ->
      let body =
        (inherit_cf ~name:typename_t ~root_type ~inh:P.inh ~synh:P.synh ~synh_root:P.synh_root) ::
        List.map (fun constr -> P.constructor root_type constr) (List.rev constrs)
      in
      [ Str.class_ [Ci.mk ~virt:Concrete ~params:(root_type.ptype_params @ (P.extra_params root_type))
                       (mknoloc plugin_name)
                       (Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this") body))
                   ]
      ]
    | _ -> failwith "Some shit happend"
  in
  type_decl_handler root_type

let str_of_type ~options ~path ({ ptype_params=type_params } as root_type) =
  let { gt_show; gt_gmap } = parse_options options in
  let _quoter = Ppx_deriving.create_quoter () in
  (* let path = Ppx_deriving.path_of_type_decl ~path root_type in *)

  let typename    = root_type.ptype_name.txt in
  let typename_t  = typename ^ "_t"  in
  let typename_tt = typename ^ "_tt" in
  let typename_meta_tt = typename ^ "_meta_tt" in
  (* let _t_typename  = "t_" ^ typename  in *)

  let show_typename_t = "show_" ^ typename_t in
  let gmap_typename_t = "gmap_" ^ typename_t in

  let derivers_bunch =
    let wrap_meth mname cname =
      let typname = root_type.ptype_name.txt in
      let body =
        wrap_with_fa ~use_lift:true [%expr GT.transform [%e Exp.ident @@ lid typname]] ~root_type
          [ Exp.new_ @@ lid cname; [%expr () ] ]
      in
      Cf.method_ (Location.mknoloc mname) Public (Cfk_concrete (Fresh, body))
    in
    [%stri let [%p Pat.var @@ mknoloc typename] =
      { GT.gcata = [%e Exp.(field (ident @@ lid typename) (lid "GT.gcata")) ]
      ; GT.plugins = [%e Exp.object_ @@ Cstr.mk (Pat.any()) @@
        (if gt_show then [wrap_meth "show" show_typename_t] else []) @
        (if gt_gmap then [wrap_meth "gmap" gmap_typename_t] else []) @
        []
        ]
      }
    ]
  in

  let make_primitive_bunch name prim_gcata =
    [%stri let [%p Pat.var @@ mknoloc name ] = { GT.gcata = [%e prim_gcata]; GT.plugins = [] }]
  in

  let show_decls = if gt_show then plugin_decls (module Show: Plugin) root_type else [] in
  let gmap_decls = if gt_gmap then plugin_decls (module Gmap: Plugin) root_type else [] in

  match root_type.ptype_kind with
  | Ptype_abstract -> begin
      match root_type.ptype_manifest with
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
            | Ptyp_var name -> (Nolabel, [%expr GT.lift [%e Exp.ident @@ lid name]])
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

      (* let _make_params_app = make_params_app_namer ~namer:(fun name -> Exp.ident @@ lid name)
      in *)
      (* let make_params_app_lift =
        make_params_app_namer ~use_lift:true
          ~namer:(fun name -> Exp.ident @@ lid name)
      in *)
      let make_params_app_fa =
        make_params_app_namer ~namer:(fun name -> Exp.ident @@ lid ("f"^name))
      in

      (* let _make_params_app first last =
         match root_type.ptype_params with
         | [] -> Exp.apply first [ (Nolabel, last) ]
         | params ->
           let res = Exp.apply first @@
             List.map (function ({ptyp_desc; _ },_) ->
               match ptyp_desc with
               | Ptyp_var name -> (Nolabel, Exp.ident @@ lid name)
               | _ -> assert false) params
           in
           Exp.apply res [(Nolabel, last)]
      in *)

      let gt_repr_typ = gt_repr_typ_wrap ~typename ~root_type [%type: unit] in
      let gt_repr_body =
        let typename_gcata = typename^"_gcata" in
        let tpo_meths =
          let f ({ptyp_desc; _},_) =
            match ptyp_desc with
            | Ptyp_var v -> Cf.method_ (mknoloc v) Public (Cfk_concrete (Fresh, Exp.ident @@ lid ("f"^v)))
            | _ -> raise_errorf "Some cases are not supported when creating tpo methods"
          in
          List.map f root_type.ptype_params
        in
        let tpo = Exp.object_ (Cstr.mk (Pat.any ()) tpo_meths ) in
        let match_body =
          Exp.match_ (Exp.ident @@ lid "subj") @@
          ListLabels.map constrs ~f:(fun { pcd_name = { txt = name' }; pcd_args } ->
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
              | _ when are_the_same arg root_type -> [%expr GT.make self [%e Exp.ident @@ lid argname] tpo]
              | Ptyp_var v -> [%expr GT.make [%e Exp.ident @@ lid @@ "f"^v] [%e Exp.ident @@ lid argname] tpo]
              | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) ->
                  [%expr [%e Exp.ident @@ lid argname]]
              | Ptyp_constr ({txt=(Lident "int"); _},[]) ->
                  [%expr [%e Exp.ident @@ lid argname]]
              | Ptyp_constr _ ->
                 [%expr [%e Exp.ident @@ lid argname]]
                 (* [%expr GT.make [%e Exp.ident @@ lid "self"] [%e Exp.ident @@ lid argname] tpo] *)
              | _ -> raise_errorf "Some cases are not supported when generating application in gcata"
            ) argnames pcd_args
            in

            Exp.case (Pat.construct (lid name') args_tuple) @@
            Exp.(apply (send (ident @@ lid "trans") (mknoloc ("c_"^name')) )
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
                let rec self = [%e make_params_app_fa (Exp.ident @@ lid typename_gcata)
                                                   [Exp.ident @@ lid "trans"] ]
                and tpo = [%e tpo ] in
                [%e match_body]
            ]]
          in
          { GT.gcata = [%e Exp.ident @@ lid typename_gcata]; GT.plugins = () }
        ]
      in

      let (tt_methods, t_methods, _) = generate_some_methods root_type constrs ~typename in
      let ans =
        [ MakeMeta.str_tt_for_algebraic ~params:root_type.ptype_params ~typename constrs
        ; make_tt_class_type ~root_type ~typename ~typename_meta_tt ~typename_tt tt_methods
        ; Str.value Nonrecursive [Vb.mk
            (Pat.(constraint_ (var @@ mknoloc typename) gt_repr_typ))
            gt_repr_body
          ]
        ; Str.class_ [Ci.mk ~virt:Virtual ~params:(default_params root_type) (mknoloc typename_t) @@
                      Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this") t_methods)
                     ]
        ]
      in
      let ans = ans @ show_decls @ gmap_decls in
      ans @ [derivers_bunch]

  | _ -> raise_errorf ~loc:root_type.ptype_loc "%s: some error2" deriver



let register () =
  Ppx_deriving.(register (create deriver
    (* ~core_type: (Ppx_deriving.with_quoter (fun quoter typ -> *)
    (*   [%expr fun x -> Format.asprintf "%a" (fun fmt -> [%e expr_of_typ quoter typ]) x])) *)

    (* TODO: maybe we not yet support recursive type definitions *)
    ~type_decl_str: (fun ~options ~path type_decls ->
      let t_descls = List.concat (List.map (str_of_type ~options ~path) type_decls) in
      t_descls
      (* t_descls @  *)
      (* logic_t -> logic_tt -> show_logic_t -> logic *)
    )
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))

let () = register ()
