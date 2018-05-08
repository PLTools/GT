open HelpersBase
open Ppxlib
open Ppxlib.Ast_builder.Default
let (@@) = Caml.(@@)

let nolabelize xs = List.map ~f:(fun x -> Asttypes.Nolabel,x) xs
let invariantize types = List.map types ~f:(fun x -> x,Asttypes.Invariant)

let lid ?(loc=Location.none) txt = { txt; loc }
let mknoloc txt = lid txt
let pexp_pair ?(loc=Location.none) a b = pexp_tuple ~loc [a; b]

let const_string ?wtf s = Pconst_string (s, wtf)

module Pat = struct
  let any ?(loc=Location.none) () = ppat_any ~loc
  let constraint_ ?(loc=Location.none) = ppat_constraint ~loc
  let construct   ?(loc=Location.none) lident pat =
    let lident = Located.mk ~loc lident in
    ppat_construct ~loc lident @@
    match pat with
    | Some {ppat_desc=Ppat_tuple [] } -> None
    | _ -> pat
  let variant ?(loc=Location.none) l pat =
    ppat_variant ~loc l @@
    match pat with
    | Some {ppat_desc=Ppat_tuple [] } -> None
    | _ -> pat

  let tuple ?(loc=Location.none) = ppat_tuple ~loc
  let var ?(loc=Location.none) s = ppat_var ~loc (Located.mk ~loc s)
  let of_string ?(loc=Location.none) s = var ~loc s
  let sprintf ?(loc=Location.none) fmt = Printf.ksprintf (of_string ~loc) fmt
  let alias ?(loc=Location.none) p s   = ppat_alias ~loc p (lid ~loc s)
  let type_ ?(loc=Location.none) lident = ppat_type ~loc lident
  let record ?(loc=Location.none) ?(flag=Closed) ps =
      ppat_record ~loc ps flag
end

module Exp = struct
  let apply ?(loc=Location.none) = pexp_apply ~loc
  let apply_nolabeled ?(loc=Location.none) e xs = pexp_apply ~loc e (nolabelize xs)
  let apply1 ?(loc=Location.none) ?(label=Nolabel) f arg = apply ~loc f [label,arg]
  let case ?guard lhs rhs = case ~lhs ~rhs ~guard
  let constant ?(loc=Location.none) = pexp_constant ~loc
  let construct ?(loc=Location.none) lident =
    pexp_construct ~loc (Located.mk ~loc lident)
  let variant ?(loc=Location.none) e ts = pexp_variant ~loc e ts
  let record ?(loc=Location.none) ?with_what ts =
    pexp_record ~loc (List.map ts ~f:(fun (l,r) -> (Located.mk ~loc l, r))) with_what

  let field ?(loc=Location.none) =
    pexp_field ~loc

  let ident ?(loc=Location.none) s = pexp_ident ~loc @@ Located.lident ~loc s
  let ident_of_long ?(loc=Location.none) l = pexp_ident ~loc (Located.mk ~loc l)
  let sprintf ?(loc=Location.none) fmt = Printf.ksprintf (ident ~loc) fmt
  let make_list ?(loc=Location.none) xs =
    List.fold_right xs
      ~f:(fun e acc ->
          construct ~loc (lident "::")
            (Some (pexp_pair ~loc e acc)) )
      ~init:(construct ~loc (lident "[]") None)
  let match_ ?(loc=Location.none) = pexp_match ~loc
  let new_ ?(loc=Location.none) = pexp_new ~loc
  let object_ ?(loc=Location.none) = pexp_object ~loc
  let tuple ?(loc=Location.none) = pexp_tuple ~loc
  let maybe_tuple ?(loc=Location.none) xs =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _   -> Some (tuple ~loc xs)

  let fun_ ?(loc=Location.none) = pexp_fun ~loc
  let fun_list ?(loc=Location.none) ~args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun arg acc -> pexp_fun ~loc Nolabel None arg acc)
  let send ?(loc=Location.none) = pexp_send ~loc
  let letmodule ?(loc=Location.none) = pexp_letmodule ~loc
  let pack_with_constraint ?(loc=Location.none) me typname =
    pexp_constraint ~loc (pexp_pack ~loc me) @@
    ptyp_package ~loc (typname, [])
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

  let constr ?(loc=Location.none) (lid: longident) ts =
    pcl_constr ~loc (Located.mk ~loc lid) ts
  let structure ?(loc=Location.none) = pcl_structure ~loc
  let let_ ?(loc=Location.none) ?(flg=Nonrecursive) = Cl.let_ ~loc flg
end


module Typ = struct
  open Ast_helper
  include Typ

  let ground  ?(loc=Location.none) s = constr (Located.lident ~loc s) []
  let class_  ?(loc=Location.none) = ptyp_class ~loc
  let constr  ?(loc=Location.none) = ptyp_constr ~loc
  let object_ ?(loc=Location.none) flg xs =
    ptyp_object ~loc (List.map xs ~f:(fun (l,r) -> Located.mk ~loc l,[],r)) flg
  let package ?(loc=Location.none) lident =
    ptyp_package ~loc (lident, [])
  let arrow ?(loc=Location.none) ?(label=Nolabel) l r =
    ptyp_arrow ~loc label l r

  let class_ ?(loc=Location.none) lident args =
    ptyp_class ~loc (Located.mk ~loc lident) args
  let chain_arrow ?(loc=Location.none) = function
    | [] -> failwith "list can't be empty"
    | xs ->
      let revxs = List.rev xs in
      List.fold_left (List.tl_exn revxs) ~init:(List.hd_exn revxs)
        ~f:(fun acc t -> arrow ~loc t acc)
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
  let constraint_ ?(loc=Location.none) t1 t2 =
    pcf_constraint ~loc (t1,t2)
  let inherit_ ?(loc=Location.none) ?(flg=Fresh) ?as_ cl_expr =
    pcf_inherit ~loc flg  cl_expr as_
  let method_ ?(loc=Location.none) name ?(flg=Public) kind =
    pcf_method ~loc (Located.mk ~loc name, flg, kind)
  let method_concrete ?(loc=Location.none) name ?(flg=Public) ?(over_flg=Fresh) e =
    method_ ~loc name ~flg (Cfk_concrete (over_flg,e))

end
module Ctf = struct
  let method_ ?(loc=Location.none) ?(flg=Public) ?(virt_flg=Virtual) name kind =
    pctf_method ~loc (Located.mk ~loc name, flg, virt_flg, kind)
  let inherit_ ?(loc=Location.none) = pctf_inherit ~loc
  let constraint_ ?(loc=Location.none) l r = pctf_constraint ~loc (l,r)
end
module Cty = struct
  include Ast_helper.Cty
  let arrow ?(loc=Location.none) ?(label=Nolabel) l r =
    Ast_helper.Cty.arrow ~loc label l r
end

module Cstr = struct
  let mk ~self fields = class_structure ~self ~fields
end

open Parsetree

let openize_poly typ =
  let loc = typ.ptyp_loc in
  Typ.variant ~loc [Rinherit typ] Open None


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


let make_new_names ?(prefix="") n =
  List.init n ~f:(fun n -> Printf.sprintf "%s_%c" prefix
    Base.Char.(of_int_exn (n + to_int 'a')) )

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
          let lhs = Pat.variant ~loc  lab @@ match args with
            | [] -> None
            | _  -> Some (Pat.tuple ~loc (List.map ~f:(Pat.var ~loc) names))
          in
          case ~guard:None ~lhs
            ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;loc},ts) ->
            let newname = "subj" in
            let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc txt)) newname
            in
            case ~guard:None ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let ls = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = Pat.alias ~loc (Pat.type_ ~loc (Located.mk ~loc (Lident lab)) ) newname
        in
        case ~guard:None ~lhs ~rhs:(onlabel lab newname)
      )
  in
  k @@ rs@ls

let prepare_patt_match ~loc what constructors make_rhs =
  let on_alg cdts =
    let k cs = Exp.match_ ~loc what cs in
    k @@ List.map cdts ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_record _ -> not_implemented "wtf"
        | Pcstr_tuple args ->
            let names = make_new_names (List.length args) in
            case ~guard:None
              ~lhs:(Pat.construct ~loc (lident cd.pcd_name.txt) @@
                    Some (Pat.tuple ~loc (Base.List.map ~f:(Pat.var ~loc) names)
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

