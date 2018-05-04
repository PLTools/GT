#load "q_MLast.cmo";;

open MLast

module Located = struct
  let mk ~loc lident = lident
end
module Pat = struct
  let any ~loc () = <:patt< _ >>
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
end

let class_structure ~self ~fields = (self, fields)

module Exp = struct
  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:expr< $lid:s$ >>) fmt
  let record ~loc lpe =
    let lpe = List.map (fun (l,r) -> Pat.of_longident ~loc l, r) lpe in
    <:expr< {$list:lpe$} >>
  let object_ ~loc (pat, fields) =
    <:expr< object ($pat$) $list:fields$ end >>
end

module Typ = struct
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
end

module Str = struct
  let single_value ~loc pat body =
    StVal (loc, Ploc.VaVal false, Ploc.VaVal [ pat,body ])

  let class_single ~loc ~name ~params fields =
    assert false
end

module Sig = struct
  let value ~loc ~name typ =
    SgVal (loc, Ploc.VaVal name, typ)
  (* let type_ ~loc recflg *)
end


module Cf = struct
  let method_concrete ~loc name body_expr =
    <:class_str_item< method $lid:name$ = $body_expr$ >>
end

let prepare_param_triples ~loc ?(extra=[]) names =
  let default_inh = Typ.var ~loc "inh" in
  let default_syn = Typ.var ~loc "syn" in

  let inh = fun ~loc s -> Typ.var ~loc ("i"^s) in
  let syn = fun ~loc s -> Typ.var ~loc ("s"^s) in
  let ps = List.concat @@ List.map (fun s ->
      [Typ.var ~loc s; inh ~loc s; syn ~loc s])
      names
  in
  ps @ [ default_inh; default_syn] @ extra


let () = ()
