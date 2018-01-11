let id x = x
let raise_errorf ?loc fmt = Printf.ksprintf failwith fmt
let not_implemented ?loc fmt =
  Printf.ksprintf (raise_errorf ~loc "%s are not yet implemented") fmt

module List = struct
  include Ppx_core.List
  let split3 xs =
    List.fold_right (fun (a,b,c) (ac,bc,cc) -> (a::ac,b::bc,c::cc))
      xs ([],[],[])
  let filter_map ~f xs =
    List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []
  let last_exn xs = List.hd @@ List.rev xs

  let pp ~f xs =
    Printf.sprintf "[ %s ]" (String.concat "; " @@ List.map f xs)

  let fold_left0 f = function
  | [] -> failwith "wrong argument of fold_left0"
  | h::tl -> fold_left ~f ~init:h tl

  let concat_map ~f xs = List.concat @@ List.map f xs
  let empty = function [] -> true | _ -> false
end

open Ppx_core
open Ppx_core.Ast_builder.Default
let (@@) = Caml.(@@)

let lid ?(loc=Location.none) txt = { txt; loc }
let mknoloc txt = lid txt
let pexp_pair ?(loc=Location.none) a b = pexp_tuple ~loc [a; b]

let const_string ?wtf s = Pconst_string (s, wtf)

module Pat = struct
  let any ?(loc=Location.none) () = ppat_any ~loc
  let constraint_ ?(loc=Location.none) = ppat_constraint ~loc
  let construct   ?(loc=Location.none) lident pat =
    match pat with
    | Some {ppat_desc=Ppat_tuple [] } -> ppat_construct ~loc lident None
    | _ -> ppat_construct ~loc lident pat

  let tuple ?(loc=Location.none) = ppat_tuple ~loc
  let var ?(loc=Location.none) lid = ppat_var ~loc lid
  let of_string ?(loc=Location.none) s = var ~loc (lid ~loc s)
  let sprintf ?(loc=Location.none) fmt = Printf.ksprintf (of_string ~loc) fmt
  let alias ?(loc=Location.none) p s   = ppat_alias ~loc p (lid ~loc s)
  let variant ?(loc=Location.none) l p = ppat_variant ~loc l p
end

module Exp = struct
  let apply ?(loc=Location.none) = pexp_apply ~loc
  let apply1 ?(loc=Location.none) ?(label=Nolabel) f arg = apply ~loc f [label,arg]
  let case ?guard lhs rhs = case ~lhs ~rhs ~guard
  let constant ?(loc=Location.none) = pexp_constant ~loc
  let construct ?(loc=Location.none) = pexp_construct ~loc

  let field ?(loc=Location.none) =
    pexp_field ~loc

  let ident ?(loc=Location.none) s = pexp_ident ~loc @@ Located.lident ~loc s
  let ident_of_long ?(loc=Location.none) l = pexp_ident ~loc l
  let sprintf ?(loc=Location.none) fmt = Printf.ksprintf (ident ~loc) fmt
  let make_list ?(loc=Location.none) xs =
    List.fold_right xs
      ~f:(fun e acc ->
          construct ~loc (lid @@ lident "::")
            (Some (pexp_pair ~loc e acc)) )
      ~init:(construct ~loc (lid @@ lident "[]") None)
  let match_ ?(loc=Location.none) = pexp_match ~loc
  let new_ ?(loc=Location.none) = pexp_new ~loc
  let object_ ?(loc=Location.none) = pexp_object ~loc
  let tuple ?(loc=Location.none) = pexp_tuple ~loc
  let fun_ ?(loc=Location.none) = pexp_fun ~loc
  let fun_list ?(loc=Location.none) ~args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun arg acc -> pexp_fun ~loc Nolabel None arg acc)
  let send ?(loc=Location.none) = pexp_send ~loc
end

module Cl = struct
  open Ast_helper
  include Cl

  let fun_list args e =
    if List.is_empty args then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ Asttypes.Nolabel None arg acc)

  let apply e args =
    if List.is_empty args then e else Cl.apply e args

  let fun_ ?(loc=Location.none) = pcl_fun ~loc

  let constr ?(loc=Location.none) (lid: longident_loc) ts =
    pcl_constr ~loc lid ts
  let structure ?(loc=Location.none) = pcl_structure ~loc
end


module Typ = struct
  open Ast_helper
  include Typ
  let ground ?(loc=Location.none) s = constr (Located.lident ~loc s) []
  let class_ ?(loc=Location.none) = ptyp_class ~loc
  let constr ?(loc=Location.none) = ptyp_constr ~loc
end

module Str = struct
  open Ast_helper
  include Str

  let single_class ?(loc=Location.none) ?(virt=Asttypes.Virtual) ?(pat=[%pat? _])
      ?(wrap= (fun x -> x)) ~name ~params body =
    Str.class_ [Ci.mk ~virt ~params (mknoloc name) @@
                wrap (Ast_helper.Cl.structure (Cstr.mk pat body))
  ]

  let class_single = single_class
end

module Cf = struct
  let constraint_ ?(loc=Location.none) t1 t2 =
    pcf_constraint ~loc (t1,t2)
  let inherit_ ?(loc=Location.none) ?(flg=Fresh) ?as_ cl_expr =
    pcf_inherit ~loc flg  cl_expr as_
  let method_ ?(loc=Location.none) name ?(flg=Public) kind =
    pcf_method ~loc (mknoloc name, flg, kind)
  let method_concrete ?(loc=Location.none) name ?(flg=Public) ?(over_flg=Fresh) e =
    method_ ~loc name ~flg (Cfk_concrete (over_flg,e))

end
module Ctf = struct
  let method_ ?(loc=Location.none) name priv_flg virt_flg kind =
    pctf_method ~loc (name, priv_flg, virt_flg, kind)
  let inherit_ ?(loc=Location.none) = pctf_inherit ~loc
end

module Cstr = struct
  let mk ~self fields = class_structure ~self ~fields
end

open Parsetree
let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")

open Longident
let affect_longident ~f = function
  | Lident x -> Lident (f x)
  | (Ldot _) as l -> l
  | (Lapply (_,_)) as l -> l

let rec map_longident ~f = function
  | Lident x -> Lident (f x)
  | Ldot (l,s) -> Ldot(l, f s)
  | Lapply (l,r) -> Lapply (l, map_longident ~f r)

let map_core_type ~onvar t =
  let rec helper t =
    match t.ptyp_desc with
    | Ptyp_any -> t
    | Ptyp_var name -> onvar name
    | Ptyp_constr (name, args) ->
        {t with ptyp_desc= Ptyp_constr (name, List.map ~f:helper args) }
    | _ -> t
  in
  helper t

module Format = struct
  include Caml.Format
  let easy_string f x =
    let (_:string) = flush_str_formatter () in
    f str_formatter x;
    flush_str_formatter ()

end

let compare_core_type a b =
  String.compare
    (Format.easy_string Pprintast.core_type a)
    (Format.easy_string Pprintast.core_type b)

let nolabelize xs = List.map ~f:(fun x -> Asttypes.Nolabel,x) xs
let invariantize types = List.map types ~f:(fun x -> x,Asttypes.Invariant)

let make_gt_a_typ ?(loc=Location.none)
    ?(inh=[%type: 'inh]) ?(itself=[%type: 'type_itself])
    ?(syn=[%type: 'syn]) ?(tpoT=[%type: 'tpoT]) () =
  (* TODO: maybe add extra string argument to concat it with type variables to get
              ('a_inh, 'a, 'a_syn, 'heck) GT.a
  *)
  [%type: ([%t inh], [%t itself], [%t syn], [%t tpoT]) GT.a]

let arr_of_param ?(loc=Location.none)
    ?(loc=Location.none) ?(inh=fun s -> Typ.var @@ "i"^s)
    ?(syn=fun s -> Typ.var @@ "s"^s) t =
  (* does from 'a the 'ia -> 'a -> 'sa *)
  match t.ptyp_desc with
  | Ptyp_var n ->
      (n, [],
        [%type: [%t inh n] -> [%t Typ.var n] -> [%t syn n]] )
  | _ ->
      failwith "arr_of_param: not all type params are supported"

let prepare_param_triples ?(loc=Location.none) ?(extra=(fun ()->[]))
    ?(normal=fun ~loc s -> Typ.var ~loc @@ s)
    ?(inh=fun ~loc s -> Typ.var ~loc @@ "i"^s)
    ?(syn=fun ~loc s -> Typ.var ~loc @@ "s"^s)
    ?(default_syn=[%type: 'syn])
    ?(default_inh=[%type: 'inh])

    params =
  let ps = List.concat @@ List.map params ~f:(fun t ->
    match t.ptyp_desc with
    | Ptyp_var n -> [normal ~loc n; inh ~loc n; syn ~loc n]
    | _ -> raise_errorf "param_triples: can't construct"
    )
  in
  let tail = [ default_inh; default_syn ] in
  ps @ (extra ()) @ tail



let params_obj ?(loc=Location.none)
    ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) root_type =
  (* converts 'a, 'b to
     < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
   *)
  let f (t,_) = arr_of_param ~inh ~syn t in
  ptyp_object ~loc (List.map ~f root_type.ptype_params) Asttypes.Closed

(* let migrate =
 *   let open Migrate_parsetree in
 *   Versions.migrate (module OCaml_405) (module OCaml_current) *)

(* let string_of_expression e =
 *   Format.set_margin 1000;
 *   let open Migrate_parsetree in
 *   let ans = Format.asprintf "%a" Pprintast.expression (migrate.Versions.copy_expression e) in
 *   Format.set_margin 80;
 *   ans *)

(* let string_of_core_type e =
 *   Format.set_margin 1000;
 *   let open Migrate_parsetree in
 *   let ans = Format.asprintf "%a" Pprintast.core_type (migrate.Versions.copy_core_type e) in
 *   Format.set_margin 80;
 *   ans *)

let using_type ~(typename: string) root_type =
  let loc = root_type.ptype_loc in
  (* generation type specification by type declaration *)
  ptyp_constr ~loc (Located.lident ~loc typename) (List.map ~f:fst @@ root_type.ptype_params)

let for_me_patt ?(loc=Location.none) () = pvar ~loc "for_me"
let for_me_expr ?(loc=Location.none) () = pexp_ident ~loc (mknoloc (Lident "for_me"))

let inh_syn_ts ?(loc=Location.none) () = [ [%type: 'inh]; [%type: 'syn] ]
(* Used when we need to check that type we working on references himself in
  it's body *)
let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Longident.Lident xxx},_) ->
    let b = (String.equal xxx tdecl.ptype_name.txt) in
    b
  | _ ->
    false
  )


let visit_typedecl ~loc
  ?(onrecord  =fun _ -> not_implemented ~loc "record types")
  ?(onmanifest=fun _ -> not_implemented ~loc "manifest")
  ?(onvariant =fun _ -> not_implemented ~loc "vairant types")
    tdecl =
  match tdecl.ptype_kind with
  | Ptype_record r -> onrecord r
  | Ptype_open     -> not_implemented ~loc "open types"
  | Ptype_variant cds -> onvariant cds
  | Ptype_abstract ->
      match tdecl.ptype_manifest with
      | None -> failwith "abstract types without manifest can't be supported"
      | Some typ -> onmanifest typ

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel =
  let k cs = Exp.match_ ~loc what cs in
  let rs =
    List.map rows ~f:(function
        | Rinherit _ -> not_implemented "inherit fields in polyvars"
        | Rtag (lab, _, _, args) ->
            let names = List.mapi args
                ~f:(fun n _ -> Char.to_string @@ Char.of_int_exn
                       (n + Char.to_int 'a'))
            in
            let lhs = Pat.variant ~loc  lab @@ match args with
              | [] -> None
              | _  -> Some (Pat.tuple (List.map ~f:(fun n -> Pat.var (mknoloc n))names))
            in
            case ~guard:None ~lhs
              ~rhs:(onrow lab @@ List.zip_exn names args)

      )
  in
  k rs

let prepare_patt_match ~loc what constructors make_rhs =
  let on_alg cdts =
    let k cs = Exp.match_ ~loc what cs in
    k @@ List.map cdts ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "wtf"
        | Pcstr_tuple ts ->
            let names = List.mapi ts
                ~f:(fun n _ -> Char.to_string @@ Char.of_int_exn
                       (n + Char.to_int 'a'))
            in
            case ~guard:None
              ~lhs:(Pat.construct ~loc (Located.lident ~loc cd.pcd_name.txt) @@
                    Some (Pat.tuple (List.map ~f:(fun n -> Pat.var (mknoloc n))names)
                         ))
              ~rhs:(make_rhs cd names)

      )
  in
  let on_poly cs =
    assert false
  in
  match constructors with
  | `Algebraic cdts -> on_alg cdts
  | `PolyVar cs -> on_poly cs
