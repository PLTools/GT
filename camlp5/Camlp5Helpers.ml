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
    | _  -> let args = <:patt< ($list:ps$) >> in
      <:patt< $c$ $args$ >>

  let type_ ~loc lident =
    PaTyp (loc, VaVal (Longident.flatten lident) )

  let record ~loc fs =
    <:patt< { $list:List.map (fun (l,r) -> (of_longident ~loc l, r) ) fs$ } >>

  let tuple ~loc ps = <:patt< ($list:ps$) >>
  let variant ~loc name args =
    let v = <:patt< ` $name$ >> in
    match args with
    | None  -> v
    | Some p -> <:patt< $v$ $p$ >>

  let alias ~loc p1 name =
    let right = <:patt< $lid:name$ >> in
    <:patt< ($p1$ as $right$) >>

  let unit ~loc = constr ~loc "()" []
end

let class_structure ~self ~fields = (self, fields)
type case = patt * expr option * expr

let case ~lhs ~rhs : case = (lhs, None, rhs)

module Exp = struct
  type t = MLast.expr

  let ident ~loc s = <:expr< $lid:s$ >>
  let lid = ident
  let uid ~loc s = <:expr< $uid:s$ >>
  let unit ~loc = lid ~loc "()"
  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:expr< $lid:s$ >>) fmt

  let string_const ~loc s = <:expr< $str:s$ >>
  let int_const ~loc n = <:expr< $int:string_of_int n$ >>
  let assert_false ~loc =
    let r = ident ~loc "false" in
    <:expr< assert $r$ >>


  let acc ~loc l r = <:expr< $l$ . $r$ >>
  let acc_list ~loc l rs = List.fold_left (acc ~loc) l rs

  let of_longident ~loc l =
    let rec helper = function
      | Longident.Lident s -> ident ~loc s
      | Ldot (l, r) ->
        let u = helper l in
        acc ~loc u (ident ~loc r)
      | _ -> assert false
    in
    helper l

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
  let new_ ~loc lident =
    <:expr< new $list: Longident.flatten lident$ >>
  let object_ ~loc (pat, fields) =
    <:expr< object ($pat$) $list:fields$ end >>
  let record ~loc lpe =
    let lpe = List.map (fun (l,r) -> Pat.of_longident ~loc l, r) lpe in
    <:expr< {$list:lpe$} >>
  let send ~loc left s = <:expr< $left$ # $s$ >>

  let from_caml e = failwith "not implemented"
end

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

  let ident ~loc s = <:ctyp< $lid:s$ >>
  let var ~loc s = <:ctyp< '$s$ >>
  let constr ~loc lident =
    let init = of_longident ~loc lident in
    function
    | []    -> init
    | x::xs -> List.fold_left (fun l r -> <:ctyp< $l$ . $r$ >>) init xs
  let app ~loc l r = <:ctyp< $l$ $r$ >>

  let of_type_arg ~loc (s,_) = match s with
    | VaVal (Some s) -> ident ~loc s
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

  let from_caml _ = assert false

  (* this might need to be changed *)
  let variant ~loc ?(is_open=false) fs =
    let vs = fs |> List.map (function
        | Ppxlib.Rinherit core_typ -> PvInh (loc, from_caml core_typ)
        | Rtag (lb, _, is_open, args) ->
          PvTag (loc, VaVal lb, VaVal is_open, VaVal (List.map from_caml args) )
      ) in
    if is_open
    then <:ctyp< [ > $list:vs$ ] >>
    else <:ctyp< [ < $list:vs$ ] >>

  let use_tdecl tdecl =
    let loc = loc_from_caml tdecl.Ppxlib.ptype_loc in
    let c = ident ~loc tdecl.ptype_name.txt in
    List.fold_left (fun acc (t,_) -> match t.Ppxlib.ptyp_desc with
        | Ptyp_var s -> app ~loc acc (var ~loc s)
        | _ -> assert false
      )
      c
      tdecl.ptype_params
end

type type_declaration = MLast.type_decl

module Str = struct
  type t = MLast.str_item
  let of_tdecls ~loc ts =
    (* <:str_item< type $list:ts$ >> *)
    (* TODO *)
    assert false
  let single_value ~loc pat body =
    StVal (loc, Ploc.VaVal false, Ploc.VaVal [ pat,body ])

  let values ~loc vbs =
    <:str_item< value rec $list:vbs$ >>
  let class_single ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = CeStr (loc, Ploc.VaVal None, Ploc.VaVal fields) }
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
end

module Sig = struct
  type t = MLast.sig_item
  let value ~loc ~name typ =
    SgVal (loc, Ploc.VaVal name, typ)
  (* let type_ ~loc recflg *)
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
  (* let apply ~loc l xs =
   *   List.fold_left (fun acc r -> <:class_expr< $acc$ $r$ >>) l  xs
   *
   *
   * let constr ~loc lident args =
   *   <:class_expr< [ $list:lt$ ] $list:ls$ >> *)
end
module Ctf = struct
  type t = class_sig_item
  let constraint_ ~loc t1 t2 = <:class_sig_item< type $t1$ = $t2$ >>
  let method_ ~loc s ?(virt=false) t =
    <:class_sig_item< method $lid:s$ : $t$ >>

  let inherit_ ~loc cty = <:class_sig_item< inherit $cty$ >>
end
(* let prepare_param_triples ~loc ?(extra=[]) names =
 *   let default_inh = Typ.var ~loc "inh" in
 *   let default_syn = Typ.var ~loc "syn" in
 *
 *   let inh = fun ~loc s -> Typ.var ~loc ("i"^s) in
 *   let syn = fun ~loc s -> Typ.var ~loc ("s"^s) in
 *   let ps = List.concat @@ List.map (fun s ->
 *       [Typ.var ~loc s; inh ~loc s; syn ~loc s])
 *       names
 *   in
 *   ps @ [ default_inh; default_syn] @ extra *)
let prepare_param_triples ~loc ?(extra=[])
    ?(inh=fun ~loc s -> "i"^s)
    ?(syn=fun ~loc s -> "s"^s)
    ?(default_syn= "inh")
    ?(default_inh= "syn")
    names : type_arg list =
  (* let default_inh = "inh" in
   * let default_syn = "syn" in *)

  (* let inh = fun ~loc s -> "i"^s in
   * let syn = fun ~loc s -> "s"^s in *)
  let ps = List.concat @@ List.map (fun s ->
      [s; inh ~loc s; syn ~loc s])
      names
  in
  let ans = List.map (fun s -> Some s)
      (ps @ [ default_inh; default_syn] @ extra)
  in
  List.map (fun p -> (Ploc.VaVal p, None)) ans


(* let invariantize xs : type_var list =
 *   List.map (fun p -> (Ploc.VaVal p,None)) xs *)



(* let make_new_names ?(prefix="") n =
 *   List.init n ~f:(fun n ->
 *     Printf.sprintf "%s_%c" prefix
 *     Base.Char.(of_int_exn (n + to_int 'a')) ) *)
