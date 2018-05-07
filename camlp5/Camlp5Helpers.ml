#load "q_MLast.cmo";;
open Ploc
open MLast

module Located = struct
  type t = Ploc.t
  let mk ~loc lident = lident
end
type loc = Located.t

type type_var = MLast.type_var

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
end

let class_structure ~self ~fields = (self, fields)

module Exp = struct
  type t = MLast.expr
  let ident ~loc s = <:expr< $lid:s$ >>
  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:expr< $lid:s$ >>) fmt
  let record ~loc lpe =
    let lpe = List.map (fun (l,r) -> Pat.of_longident ~loc l, r) lpe in
    <:expr< {$list:lpe$} >>
  let object_ ~loc (pat, fields) =
    <:expr< object ($pat$) $list:fields$ end >>
  let send ~loc left s =
    <:expr< $left$ # $s$ >>
  let app ~loc l r = <:expr< $l$ $r$ >>
  let app_list ~loc l xs =
    List.fold_left (app ~loc) l xs
  let match_ ~loc e xs =
    let xs = List.map (fun (a,b,c) -> (a, Ploc.VaVal b, c)) xs in
    <:expr< match $e$ with [ $list:xs$ ] >>

  let fun_ ~loc pat e =
    <:expr< fun [ $list:[ (pat,Ploc.VaVal None,e) ]$ ] >>

  let fun_list ~loc pats body =
    List.fold_right (fun x acc -> fun_ ~loc x acc) pats body
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

  let var ~loc s = <:ctyp< '$s$ >>
  let constr ~loc lident =
    let init = of_longident ~loc lident in
    function
    | []    -> init
    | x::xs -> List.fold_left (fun l r -> <:ctyp< $l$ . $r$ >>) init xs

  let object_ ~loc flg lst =
    <:ctyp< < $list:lst$ $flag:(match flg with Ppxlib.Open -> true | Ppxlib.Closed -> false)$ > >>
  let arrow ~loc t1 t2 = <:ctyp< $t1$ -> $t2$ >>
  let chain_arrow ~loc = function
  | [] -> assert false
  | xs -> let r = List.rev xs in
      let init = List.hd r in
      List.fold_left (fun acc x -> arrow ~loc x acc) init (List.tl r)
end

module Str = struct
  type t = MLast.str_item
  let of_tdecls ~loc ts = <:str_item< type $list:ts$ >>
  let single_value ~loc pat body =
    StVal (loc, Ploc.VaVal false, Ploc.VaVal [ pat,body ])

  let class_single ~loc ~name ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal false;
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


module Cf = struct
  type t = MLast.class_str_item
  let method_concrete ~loc name body_expr =
    <:class_str_item< method $lid:name$ = $body_expr$ >>

  let method_virtual ~loc name typ =
    <:class_str_item< method virtual $lid:name$ : $typ$ >>
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
let prepare_param_triples ~loc ?(extra=[]) names =
  let default_inh = "inh" in
  let default_syn = "syn" in

  let inh = fun ~loc s -> "i"^s in
  let syn = fun ~loc s -> "s"^s in
  let ps = List.concat @@ List.map (fun s ->
      [s; inh ~loc s; syn ~loc s])
      names
  in
  let ans = List.map (fun s -> Some s)
      (ps @ [ default_inh; default_syn] @ extra)
  in
  ans

let invariantize xs : type_var list =
  List.map (fun p -> (Ploc.VaVal p,None)) xs


let () = ()

(* let make_new_names ?(prefix="") n =
 *   List.init n ~f:(fun n ->
 *     Printf.sprintf "%s_%c" prefix
 *     Base.Char.(of_int_exn (n + to_int 'a')) ) *)
