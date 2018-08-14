#load "q_MLast.cmo";;

open Ploc
open MLast

module Located = struct
  type t = Ploc.t
  let mk ~loc lident = lident
end
type loc = Located.t

let loc_from_caml camlloc =
  let open Ppxlib.Location in
  let { loc_start; loc_end} = camlloc in
  Ploc.make_loc loc_start.pos_fname loc_start.pos_lnum loc_start.pos_bol
    (loc_start.pos_bol, loc_start.pos_bol + loc_start.pos_cnum) ""

let noloc = Ploc.dummy

type type_arg = MLast.type_var
let named_type_arg ~loc s : type_arg = (Ploc.VaVal (Some s), None)

module Pat = struct
  type t = MLast.patt
  let any ~loc () = <:patt< _ >>
  let var ~loc s  = <:patt< $lid:s$ >>
  let sprintf ~loc fmt = Printf.ksprintf (fun s -> <:patt< $lid:s$ >>) fmt
  let of_longident ~loc lid =
    let open Ppxlib.Longident in
    let wrap s =
      let c1 = String.get s 0 in
      if Char.(equal (uppercase_ascii c1) c1)
      then <:patt< $uid:s$ >>
      else <:patt< $lid:s$ >>
    in
    let rec helper = function
      | Lident s -> wrap s
      | Ldot (left, s) -> <:patt< $helper left$ . $wrap s$ >>
      | _ -> assert false
    in
    helper lid


  let constr ~loc uid ps =
    let c = <:patt< $uid:uid$ >> in
    match ps with
    | [] -> c
    | [x] -> <:patt< $c$ $x$ >>
    | _  -> let args = <:patt< ($list:ps$) >> in
      <:patt< $c$ $args$ >>

  let type_ ~loc lident =
    PaTyp (loc, VaVal (Longident.flatten lident) )

  let record ~loc fs =
    <:patt< { $list:List.map (fun (l,r) -> (of_longident ~loc l, r) ) fs$ } >>
  let record1 ~loc lident expr = record ~loc [lident, expr]

  let tuple ~loc ps = <:patt< ($list:ps$) >>
  let variant ~loc name args =
    let v = <:patt< ` $name$ >> in
    match args with
    | []  -> v
    | [x] -> <:patt< $v$ $x$ >>
    | _ ->
      let tup = tuple ~loc args in
      <:patt< $v$ $tup$ >>

  let alias ~loc p1 name =
    let right = <:patt< $lid:name$ >> in
    <:patt< ($p1$ as $right$) >>

  let unit ~loc =     <:patt< () >> (* constr ~loc "()" [] *)
end


type case = patt * expr option * expr
let case ~lhs ~rhs : case = (lhs, None, rhs)


module Exp = struct
  type t = MLast.expr

  let ident ~loc s =
    if Base.Char.is_uppercase s.[0]
    then <:expr< $uid:s$ >>
    else <:expr< $lid:s$ >>
  let lid = ident
  (* let uid ~loc s = <:expr< $uid:s$ >> *)
  let unit ~loc =  <:expr< () >>
  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:expr< $lid:s$ >>) fmt

  let string_const ~loc s = <:expr< $str:s$ >>
  let int_const ~loc n = <:expr< $int:string_of_int n$ >>
  let assert_false ~loc =
    let r = ident ~loc "false" in
    <:expr< assert $r$ >>

  let of_longident ~loc l =
    let rec helper = function
      (* | Longident.Lident s when Char.equal s.[0] (Char.uppercase_ascii s.[0]) -> uid ~loc s *)
      | Longident.Lident s -> ident ~loc s
      | Ldot (l, r) ->
        let u = helper l in
        <:expr< $u$ . $ident ~loc r$ >>
        (* acc ~loc u (ident ~loc r) *)
      | _ -> assert false
    in
    helper l

  let acc ~loc e l = <:expr< $e$ . $of_longident ~loc l$ >>
  (* let acc_list ~loc l rs = List.fold_left (acc ~loc) l rs *)


  let app ~loc l r = <:expr< $l$ $r$ >>
  let app_list ~loc l xs =
    List.fold_left (app ~loc) l xs
  let match_ ~loc e (xs: case list) =
    let xs = List.map (fun (a,b,c) -> (a, Ploc.VaVal b, c)) xs in
    <:expr< match $e$ with [ $list:xs$ ] >>

  let fun_ ~loc pat e =
    <:expr< fun [ $list:[ (pat,Ploc.VaVal None,e) ]$ ] >>
  let fun_list ~loc pats body =
    List.fold_right (fun x acc -> fun_ ~loc x acc) pats body

  let construct ~loc lident args =
    app_list ~loc (of_longident ~loc lident) args
  let variant ~loc s args =
    app_list ~loc <:expr< ` $s$ >> args
  let tuple ~loc le = <:expr< ($list:le$) >>

  let new_ ~loc lident =
    <:expr< new $list: Longident.flatten lident$ >>
  let object_ ~loc (pat, fields) =
    <:expr< object ($pat$) $list:fields$ end >>
  let send ~loc left s = <:expr< $left$ # $s$ >>

  let record ~loc lpe =
    let lpe = List.map (fun (l,r) -> Pat.of_longident ~loc l, r) lpe in
    <:expr< {$list:lpe$} >>
  let record1 ~loc lident expr = record ~loc [lident, expr]

  let field ~loc e lident = acc ~loc e lident
  let let_ ~loc lpe ewhere = <:expr< let $list:lpe$ in $ewhere$ >>
  let let_one ~loc pat e1 ewhere = let_ ~loc [pat, e1] ewhere

  let from_caml e = failwith "not implemented"
  let assert_false ~loc = <:expr< assert False >>
  let failwith_ ~loc s  = <:expr< failwith $str:s$ >>
  let objmagic_unit ~loc = <:expr< Obj.magic () >>
  let true_  ~loc = <:expr< True >>
  let false_ ~loc = <:expr< False >>

  let list ~loc xs =
    let rec helper acc = function
      | [] -> acc
      | x::xs -> helper (app_list ~loc <:expr< $uid:"::"$ >> [x; acc]) xs
    in
    helper <:expr< $uid:"[]"$ >> (List.rev xs)
end

type lab_decl = (loc * string * bool * ctyp)
let lab_decl ~loc name is_mut typ = (loc, name, is_mut, typ)

module Typ = struct
  type t = MLast.ctyp

  let of_longident ~loc lid =
    let open Ppxlib.Longident in
    let wrap s =
      let c1 = String.get s 0 in
      if Char.(equal (uppercase_ascii c1) c1)
      then <:ctyp< $uid:s$ >>
      else <:ctyp< $lid:s$ >>
    in
    let rec helper = function
      | Lident s -> wrap s
      | Ldot (left, s) -> <:ctyp< $helper left$ . $wrap s$ >>
      | _ -> assert false
    in
    helper lid

  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:ctyp< $lid:s$ >>) fmt
  let ident ~loc s = <:ctyp< $lid:s$ >>
  let var ~loc s = <:ctyp< '$s$ >>
  let app ~loc l r = <:ctyp< $l$ $r$ >>
  let any ~loc  = <:ctyp< _ >>
  let alias ~loc t s =
    let p = var ~loc s in
    <:ctyp< $t$ as $p$ >>
  let tuple ~loc lt = <:ctyp< ( $list:lt$ ) >>
  let constr ~loc lident =
    let init = of_longident ~loc lident in
    function
    | []    -> init
    | lt ->
      List.fold_left (app ~loc) init lt

  let class_ ~loc lident  =
    let init = <:ctyp< # $list:Longident.flatten lident$ >> in
    function
    | []    -> init
    (* | [r]   -> <:ctyp< $init$ $r$ >> *)
    | lt ->
      List.fold_left (app ~loc) init lt

  let of_type_arg ~loc (s,_) = match s with
    | VaVal (Some s) -> var ~loc s
    | VaAnt _ -> assert false
    | VaVal None -> failwith "bad type arg"

  let object_ ~loc flg lst =
    <:ctyp< < $list:lst$ $flag:(match flg with Ppxlib.Open -> true | Ppxlib.Closed -> false)$ > >>
  let arrow ~loc t1 t2 = <:ctyp< $t1$ -> $t2$ >>
  let chain_arrow ~loc = function
  | [] -> assert false
  | xs ->
    let r = List.rev xs in
    let init = List.hd r in
    List.fold_left (fun acc x -> arrow ~loc x acc) init (List.tl r)

  let from_caml root_typ =
    let rec helper typ =
      let loc = loc_from_caml typ.Ppxlib.ptyp_loc in
      match typ.ptyp_desc with
      | Ptyp_any   -> <:ctyp< _ >>
      | Ptyp_var s -> <:ctyp< '$s$ >>
      | Ptyp_arrow (lab, l, r) -> arrow ~loc (helper l) (helper r)
      | Ptyp_constr ({txt;_}, ts) -> constr ~loc txt (List.map helper ts)
      | Ptyp_tuple ts -> <:ctyp< ( $list:(List.map helper ts)$ ) >>
      | _ -> failwith "Not implemented: conversion from OCaml ast to Camlp5 Ast"
    in
    helper root_typ
  (* this might need to be changed *)
  let variant ~loc ?(is_open=false) fs =
    let vs = fs |> List.map (function
        | Ppxlib.Rinherit core_typ -> PvInh (loc, from_caml core_typ)
        | Rtag (lb, _, is_open, args) ->
          PvTag (loc, VaVal lb.txt, VaVal is_open, VaVal (List.map from_caml args) )
      ) in
    if is_open
    then <:ctyp< [ > $list:vs$ ] >>
    else <:ctyp< [ < $list:vs$ ] >>

  let variant_of_t ~loc typ =
    <:ctyp< [ > $list:[PvInh (loc, typ)]$ ] >>
  let use_tdecl tdecl =
    let loc = loc_from_caml tdecl.Ppxlib.ptype_loc in
    let c = ident ~loc tdecl.ptype_name.txt in
    List.fold_left (fun acc (t,_) -> match t.Ppxlib.ptyp_desc with
        | Ptyp_var s -> app ~loc acc (var ~loc s)
        | _ -> assert false
      )
      c
      tdecl.ptype_params

  let poly ~loc names t = <:ctyp< ! $list:names$ . $t$ >>

  let map ~onvar t = t
end

type type_declaration = MLast.type_decl
type class_declaration = class_expr class_infos

let class_declaration  ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CeStr (loc, Ploc.VaVal None, Ploc.VaVal fields) }
    in
    c

module Str = struct
  type t = MLast.str_item
  let of_tdecls ~loc td =
    let open Ppxlib in
    let tdPrm = HelpersBase.map_type_param_names td.ptype_params
        ~f:(fun s -> named_type_arg ~loc s)
    in
    let tdDef =
      match td.ptype_kind with
      | Ptype_variant cds ->
        let llslt = List.map (fun cd ->
            let args =
              match cd.pcd_args with
              | Pcstr_record _ -> assert false
              | Pcstr_tuple ts ->  List.map Typ.from_caml ts
            in
            (loc, VaVal cd.pcd_name.txt, VaVal args, None)
          ) cds
        in
        MLast.TySum (loc, Ploc.VaVal llslt)
      | _ -> assert false

    in
    let t =
      { tdNam = VaVal (loc, VaVal td.ptype_name.txt);
        tdPrm = VaVal tdPrm;
        tdPrv = VaVal false;
        tdDef;
        tdCon = VaVal []
      }
    in
    <:str_item< type $list:[t]$ >>
    (* TODO *)
    (* assert false *)
  let single_value ~loc pat body =
    StVal (loc, Ploc.VaVal false, Ploc.VaVal [ pat,body ])

  let values ~loc vbs =
    <:str_item< value rec $list:vbs$ >>
  let class_single ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CeStr (loc, Ploc.VaVal None, Ploc.VaVal fields) }
    in

    <:str_item< class $list:[c]$ >>

  let tdecl ~loc ~name ~params rhs =
    let t = { tdNam = VaVal (loc, VaVal name)
            ; tdPrm = VaVal (List.map (fun s -> (VaVal (Some s),None)) params)
            ; tdPrv = VaVal false
            ; tdDef = rhs
            ; tdCon = VaVal []
            }
    in
    <:str_item< type $list:[t]$ >>

  let of_class_declarations ~loc (lcice: class_declaration list) =
    <:str_item< class $list:lcice$ >>

  let tdecl_record ~loc ~name ~params llsbt =
    let t = <:ctyp< { $list:llsbt$ } >> in
    tdecl ~loc ~name ~params t

end

module Sig = struct
  type t = MLast.sig_item
  let value ~loc ~name typ =
    SgVal (loc, Ploc.VaVal name, typ)
    (* let type_ ~loc recflg *)

  let class_ ~loc ~name ~params ?(virt=false) ?(wrap=(fun x -> x)) fields =
    (* TODO: wrap *)
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CtSig (loc, Ploc.VaVal None, Ploc.VaVal fields)
            }
    in
    <:sig_item< class $list:[c]$ >>
end

module Vb = struct
  type t = Pat.t * Exp.t
end

let value_binding ~loc ~pat ~expr = (pat, expr)

module Cf = struct
  type t = MLast.class_str_item
  let method_concrete ~loc name body_expr =
    <:class_str_item< method $lid:name$ = $body_expr$ >>

  let method_virtual ~loc name typ =
    <:class_str_item< method virtual $lid:name$ : $typ$ >>

  let inherit_ ~loc ?(as_=None) ce =
    <:class_str_item< inherit $ce$ $opt:as_$ >>
  let constraint_ ~loc t1 t2 = <:class_str_item< type $t1$ = $t2$ >>
end
module Cty = struct
  type t = class_type
  let acc ~loc ct1 ct2 = <:class_type< $ct1$ . $ct2$ >>
  let id ~loc s = <:class_type< $id:s$ >>
  let of_longident ~loc l =
    let rec helper = function
      | Ppxlib.Lident s -> id ~loc s
      | Ldot (l, s) -> acc ~loc (helper l) (id ~loc s)
      | _ -> assert false
    in
    helper l

  let arrow ~loc t ct = <:class_type< [ $t$ ] -> $ct$ >>
  let constr ~loc longident lt =
    let ct = of_longident ~loc longident in
    <:class_type< $ct$ [ $list:lt$ ] >>
end
module Cl = struct
  type t = class_expr
  let constr ~loc lident args =
    let ls = Longident.flatten lident in
    <:class_expr< [ $list:args$ ] $list:ls$ >>
  let apply ~loc l xs =
    List.fold_left (fun acc r -> <:class_expr< $acc$ $r$ >>) l  xs
  let fun_ ~loc p ce = <:class_expr< fun $p$ -> $ce$ >>
  let fun_list ~loc ps ce =
    List.fold_right (fun_ ~loc) ps ce


end
module Ctf = struct
  type t = class_sig_item
  let constraint_ ~loc t1 t2 = <:class_sig_item< type $t1$ = $t2$ >>
  let method_ ~loc ?(virt=false) s t =
    if virt
    then <:class_sig_item< method virtual $lid:s$ : $t$ >>
    else <:class_sig_item< method $lid:s$ : $t$ >>

  let inherit_ ~loc cty = <:class_sig_item< inherit $cty$ >>
end

let class_structure ~self ~fields = (self, fields)
type class_structure = Pat.t * Cf.t list

let typ_arg_of_core_type t =
  match t.Ppxlib.ptyp_desc with
  | Ptyp_any -> failwith "wildcards are not supported"
  | Ptyp_var s -> named_type_arg ~loc:(loc_from_caml t.ptyp_loc) s
  | _ -> assert false

let openize_poly t =
  match t with
  | MLast.TyVrn (loc, name, flg) ->
    (fun flg -> MLast.TyVrn (loc, name, flg) )
      (
      match flg with
      | None -> Some None
      | Some None -> Some None
      | Some (Some xs) -> Some (Some xs)
    )
  | t -> t

(* Need to be synchronized with Expander.params_of_interface_class *)
let prepare_param_triples ~loc ~extra
    ?(inh=fun ~loc:loc s -> Typ.var ~loc @@ "i" ^ s)
    ?(syn=fun ~loc:loc s -> Typ.var ~loc @@ "s" ^ s)
    ?(default_inh = Typ.var ~loc "syn")
    ?(default_syn = Typ.var ~loc "inh")
    names  =

  let ps = List.concat @@ List.map (fun s ->
      [ inh ~loc s; Typ.var ~loc s; syn ~loc s])
      names
  in
  ps @ [ default_inh; extra; default_syn]

let typ_vars_of_typ t = []
