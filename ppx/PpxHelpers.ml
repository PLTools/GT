open Ppxlib
open Base
open HelpersBase
open Ppxlib.Ast_builder.Default
let (@@) = Caml.(@@)

let nolabelize xs = List.map ~f:(fun x -> Asttypes.Nolabel,x) xs
let invariantize types = List.map types ~f:(fun x -> x,Asttypes.Invariant)

type loc = Location.t
let noloc = Location.none
let loc_from_caml l = l

type type_arg = core_type
let named_type_arg ~loc s = ptyp_var ~loc s
let typ_arg_of_core_type t = t

let lid ?(loc=Location.none) txt = { txt; loc }
let mknoloc txt = lid txt
let pexp_pair ?(loc=Location.none) a b = pexp_tuple ~loc [a; b]

let const_string ?wtf s = Pconst_string (s, wtf)

module Pat = struct
  type t = pattern

  let any ~loc () = ppat_any ~loc
  let unit ~loc = [%pat? () ]
  let of_longident ~loc lident =
    let rec helper = function
      | Lident s -> ppat_var ~loc (Located.mk ~loc s)
      | _ -> assert false
    in
    helper lident
  let constraint_ ~loc = ppat_constraint ~loc
  let constr      ~loc lident ps =
    let lident = Located.lident ~loc lident in
    ppat_construct ~loc lident @@
    match ps with
    | [] -> None
    | _  -> Some (ppat_tuple ~loc ps)
  let variant ~loc l ps =
    ppat_variant ~loc l @@
    match ps with
    | [] -> None
    | _  -> Some (ppat_tuple ~loc ps)

  let tuple ~loc = ppat_tuple ~loc
  let var ~loc s = ppat_var ~loc (Located.mk ~loc s)
  let of_string ~loc s = var ~loc s
  let sprintf ~loc fmt = Printf.ksprintf (of_string ~loc) fmt
  let alias ~loc p s   = ppat_alias ~loc p (lid ~loc s)
  let type_ ~loc lident = ppat_type ~loc (Located.mk ~loc lident)
  let record ~loc ps =
    ppat_record ~loc (List.map ~f:(fun (l,t) -> (Located.mk ~loc l, t)) ps) Closed
end

module Exp = struct
  type t = expression
  let from_caml e = e

  let ident ~loc s = pexp_ident ~loc @@ Located.lident ~loc s
  let of_longident ~loc l = pexp_ident ~loc (Located.mk ~loc l)
  let sprintf ~loc fmt = Printf.ksprintf (ident ~loc) fmt

  let unit ~loc = [%expr ()]
  let uid  ~loc = assert false
  let lid = ident

  let constant ~loc = pexp_constant ~loc
  let int_const ~loc n = constant ~loc (Pconst_integer (Int.to_string n, None))
  let string_const ~loc s = constant ~loc (Pconst_string (s, None))

  let app ~loc l r = pexp_apply ~loc l [(Nolabel, r)]
  let app_list ~loc e xs = pexp_apply ~loc e (nolabelize xs)
  (* let apply1 ~loc ?(label=Nolabel) f arg = pexp_apply ~loc f [label,arg] *)
  let field ~loc t lident =
    pexp_field ~loc t (Located.mk ~loc lident)

  let acc ~loc l r = assert false
  let acc_list ~loc l xs = assert false
  let fun_ ~loc = pexp_fun ~loc Nolabel None
  let fun_list ~loc args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun arg acc -> pexp_fun ~loc Nolabel None arg acc)

  let case ?guard lhs rhs = case ~lhs ~rhs ~guard

  let record ~loc ts =
    pexp_record ~loc (List.map ts ~f:(fun (l,r) -> (Located.mk ~loc l, r))) None

  let construct ~loc lident xs =
    pexp_construct ~loc (Located.mk ~loc lident) @@
    match xs with
    | [] -> None
    | xs -> Some (pexp_tuple ~loc xs)

  let variant ~loc e ts =
    match ts with
    | [] -> pexp_variant ~loc e None
    | _  -> pexp_variant ~loc e (Some (pexp_tuple ~loc ts))


  (* let make_list ~loc xs =
   *   List.fold_right xs
   *     ~f:(fun e acc -> construct ~loc (lident "::") (pexp_tuple ~loc [e; acc])
   *     ~init:(construct ~loc (lident "[]") None) *)
  let match_ ~loc = pexp_match ~loc
  let new_ ~loc s = pexp_new ~loc (Located.mk ~loc s)
  let object_ ~loc = pexp_object ~loc
  let tuple ~loc = pexp_tuple ~loc
  let maybe_tuple ~loc xs =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _   -> Some (tuple ~loc xs)

  let send ~loc obj s = pexp_send ~loc obj (Located.mk ~loc s)
  let letmodule ~loc = pexp_letmodule ~loc
  let pack_with_constraint ~loc me typname =
    pexp_constraint ~loc (pexp_pack ~loc me) @@
    ptyp_package ~loc (typname, [])

  let assert_false ~loc = [%expr assert false]
  let objmagic_unit ~loc = [%expr Obj.magic ()]
  let failwith_ ~loc s = app ~loc [%expr failwith] (string_const ~loc s)

end

module Cl = struct
  open Ast_helper
  include Cl

  type t = class_expr
  let fun_list args e =
    if List.is_empty args then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ Asttypes.Nolabel None arg acc)

  let apply e args =
    if List.is_empty args then e else Cl.apply e args

  let fun_ ?(loc=Location.none) = pcl_fun ~loc

  let constr ?(loc=Location.none) (lid: longident) ts =
    pcl_constr ~loc (Located.mk ~loc lid) ts
  let structure ?(loc=Location.none) = pcl_structure ~loc
  let let_ ?(loc=Location.none) ?(flg=Nonrecursive) = Cl.let_ ~loc flg
end


module Typ = struct
  open Ast_helper
  (* include Typ *)

  type t = Ppxlib.core_type
  let constr ~loc lident = ptyp_constr ~loc (Located.mk ~loc lident)

  let of_type_arg ~loc typ = {typ with ptyp_loc = loc}
  let from_caml typ = typ
  let use_tdecl tdecl =
    let loc = tdecl.ptype_loc in
    ptyp_constr ~loc (Located.lident ~loc tdecl.ptype_name.txt) @@
    (List.map ~f:fst tdecl.ptype_params)

  let ident  ~loc s = ptyp_constr ~loc (Located.lident ~loc s) []
  let sprintf ~loc fmt = Printf.ksprintf (ident ~loc) fmt
  let of_longident ~loc lident = ptyp_constr ~loc (Located.mk ~loc lident) []

  let var ~loc s = ptyp_var ~loc s
  let any ~loc = ptyp_any ~loc
  (* let ground  ~loc s = constr ~loc (Located.mk ~loc s) [] *)
  let class_  ~loc = ptyp_class ~loc
  let object_ ~loc flg xs =
    ptyp_object ~loc (List.map xs ~f:(fun (l,r) -> Located.mk ~loc l,[],r)) flg
  let package ~loc lident =
    ptyp_package ~loc (lident, [])
  let arrow ~loc l r =
    ptyp_arrow ~loc Nolabel l r

  let class_ ~loc lident args =
    ptyp_class ~loc (Located.mk ~loc lident) args
  let chain_arrow ~loc = function
    | [] -> failwith "list can't be empty"
    | xs ->
      let revxs = List.rev xs in
      List.fold_left (List.tl_exn revxs) ~init:(List.hd_exn revxs)
        ~f:(fun acc t -> arrow ~loc t acc)

  let variant ~loc ?(is_open=false) fields =
    ptyp_variant ~loc fields (if is_open then Open else Closed) None

  let variant_of_t ~loc t = [%type: [> [%t t] ] ]
  let alias ~loc t s = ptyp_alias ~loc t s
end

module Str = struct
  open Ast_helper
  include Str

  let single_class ?(loc=Location.none) ?(virt=Asttypes.Virtual) ?(pat=[%pat? _])
      ?(wrap= (fun x -> x)) ~name ~params body =
    Str.class_ [Ci.mk ~virt ~params (Located.mk ~loc name) @@
                wrap (Ast_helper.Cl.structure (Cstr.mk pat body))
  ]

  (* make value have a default re4cursive flag *)
  let class_single = single_class

  let value ?(loc=Location.none) ?(flag=Nonrecursive) decls =
    pstr_value ~loc flag decls
  let single_value ?(loc=Location.none) ?(flag=Nonrecursive) pat expr =
    pstr_value ~loc flag [value_binding ~loc ~pat ~expr]
end

module Sig = struct
  open Ast_helper
  include Sig
  let class_ ?(loc=Location.none) ?(virt=Asttypes.Virtual)
      ?(wrap= (fun x -> x)) ~name ~params body =
    psig_class ~loc [Ci.mk ~loc (Located.mk ~loc name) ~virt ~params @@
                     wrap (Cty.signature (Csig.mk [%type: _] body))
                    ]

  let value ?(loc=Location.none) ?(prim=[]) ~name type_ = psig_value ~loc @@
    value_description ~loc ~name:(Located.mk ~loc name) ~type_ ~prim

end
module Cf = struct
  type t = class_field
  let constraint_ ~loc t1 t2 =
    pcf_constraint ~loc (t1,t2)
  let inherit_ ~loc ?(as_=None) cl_expr =
    let (_: string option) = as_ in
    let flg = Fresh in
    pcf_inherit ~loc flg  cl_expr @@ Option.map ~f:(fun s -> Located.mk ~loc s) as_
  let method_ ~loc name ?(flg=Public) kind =
    pcf_method ~loc (Located.mk ~loc name, flg, kind)
  let method_concrete ~loc name (* ?(flg=Public) ?(over_flg=Fresh) *) e =
    method_ ~loc name ~flg:Public (Cfk_concrete (Fresh, e))
  let method_virtual ~loc name (* ?(flg=Public) *) typ =
    method_ ~loc name ~flg:Public (Cfk_virtual typ)

end
module Ctf = struct
  let method_ ~loc ?(flg=Public) ?(virt_flg=Virtual) name kind =
    pctf_method ~loc (Located.mk ~loc name, flg, virt_flg, kind)
  let inherit_ ~loc = pctf_inherit ~loc
  let constraint_ ~loc l r = pctf_constraint ~loc (l,r)
end
module Cty = struct
  include Ast_helper.Cty
  type t = class_type
  let arrow ~loc  l r =
    Ast_helper.Cty.arrow ~loc Nolabel l r
end

module Vb = struct

end
let value_binding ~loc ~pat ~expr =
  value_binding
module Cstr = struct
  let mk ~self fields = class_structure ~self ~fields
end

type case = Ppxlib.case
let case  ~lhs ~rhs = 1

type class_structure = Ppxlib.class_structure
let class_structure ~self ~fields = (self,fields)

open Parsetree

let openize_poly typ =
  let loc = typ.ptyp_loc in
  Typ.variant ~loc ~is_open:true [Rinherit typ]


let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")

let prepare_param_triples ?(loc=Location.none) ?(extra=(fun ()->[]))
    ?(normal=fun ~loc s -> Typ.var ~loc @@ s)
    ?(inh=fun ~loc s -> Typ.var ~loc @@ "i"^s)
    ?(syn=fun ~loc s -> Typ.var ~loc @@ "s"^s)
    ?(default_syn=[%type: 'syn])
    ?(default_inh=[%type: 'inh])
    ?(middle=[])
    params =
  let ps = List.concat @@ map_type_param_names params ~f:(fun n ->
    [normal ~loc n; inh ~loc n; syn ~loc n]
  )
  in
  let tail = [ default_inh; default_syn ] in
  ps @ (extra ()) @ tail @ middle


(* let params_obj ?(loc=Location.none)
 *     ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) root_type =
 *   (\* converts 'a, 'b to
 *      < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
 *    *\)
 *   let f (t,_) = arr_of_param ~inh ~syn t in
 *   ptyp_object ~loc (List.map ~f root_type.ptype_params) Asttypes.Closed *)

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


(* let make_new_names ?(prefix="") n =
 *   List.init n ~f:(fun n -> Printf.sprintf "%s_%c" prefix
 *     Base.Char.(of_int_exn (n + to_int 'a')) )
 *
 * let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
 *   let k cs = Exp.match_ ~loc what cs in
 *   let rs =
 *     List.map rows ~f:(function
 *         | Rtag (lab, _, _, args) ->
 *           let args = match args with
 *             | [t] -> unfold_tuple t
 *             | [] -> []
 *             | _ -> failwith "we don't support conjunction types"
 *           in
 *           let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
 *           let lhs = Pat.variant ~loc  lab @@ match args with
 *             | [] -> None
 *             | _  -> Some (Pat.tuple ~loc (List.map ~f:(Pat.var ~loc) names))
 *           in
 *           case ~guard:None ~lhs
 *             ~rhs:(onrow lab @@ List.zip_exn names args)
 *         | Rinherit typ ->
 *           match typ.ptyp_desc with
 *           | Ptyp_constr({txt;loc},ts) ->
 *             let newname = "subj" in
 *             let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc txt)) newname
 *             in
 *             case ~guard:None ~lhs ~rhs:(oninherit ts txt newname)
 *           | _ -> failwith "this inherit field isn't supported"
 *
 *       )
 *   in
 *   let ls = match labels with
 *     | None -> []
 *     | Some ls -> List.map ls ~f:(fun lab ->
 *         let newname = "subj" in
 *         let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc (Lident lab)) ) newname
 *         in
 *         case ~guard:None ~lhs ~rhs:(onlabel lab newname)
 *       )
 *   in
 *   k @@ rs@ls
 *
 * let prepare_patt_match ~loc what constructors make_rhs =
 *   let on_alg cdts =
 *     let k cs = Exp.match_ ~loc what cs in
 *     k @@ List.map cdts ~f:(fun cd ->
 *         match cd.pcd_args with
 *         | Pcstr_record _ -> not_implemented "wtf"
 *         | Pcstr_tuple args ->
 *             let names = make_new_names (List.length args) in
 *             case ~guard:None
 *               ~lhs:(Pat.construct ~loc (lident cd.pcd_name.txt) @@
 *                     Some (Pat.tuple ~loc (Base.List.map ~f:(Pat.var ~loc) names)
 *                          ))
 *               ~rhs:(make_rhs cd names)
 *
 *       )
 *   in
 *   let on_poly cs =
 *     assert false
 *   in
 *   match constructors with
 *   | `Algebraic cdts -> on_alg cdts
 *   | `PolyVar cs -> on_poly cs *)
