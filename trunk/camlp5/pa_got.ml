
(**************************************************************************
 *  Copyright (C) 2012-2013
 *  Dmitri Boulytchev (dboulytchev@math.spbu.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open List
open Printf
open Pcaml
open MLast
open Ploc
     
let get_val (VaVal x) = x 

module S = Set.Make (String)

let name_generator list =
  let s = ref (fold_right S.add list S.empty) in
  object(self)
    method generate prompt =
      if S.mem prompt !s 
      then self#generate ("_" ^ prompt)
      else (
        s := S.add prompt !s;
        prompt
      )
  end

let cata  name  = name ^ "_gcata"
let others      = "others"
let cmethod c   = "m_" ^ c
let apply       = "apply"
let closed name = "closed_" ^ name
    
EXTEND
  GLOBAL: str_item; 

  str_item: LEVEL "top" [
    [ "generic"; t=LIST1 t_decl SEP "and" -> 
      let make_call g f args =
        fold_left (fun e a -> <:expr< $e$ $g a$ >>) f args
      in
      let make_fun g args body =
        fold_right 
          (fun arg expr -> <:expr< fun [ $list:[g arg, VaVal None, expr]$ ] >>)                  
          args
          body
      in
      let id x = x in
      let t, d = split   t in
      let t    = flatten t in
      let get_cata =
        let s = fold_left (fun s (_, n, _) -> S.add n s) S.empty d in
        fun name -> 
          if S.mem name s then <:expr< $lid:cata name$ >> 
                          else let name, gcata = <:expr< $lid:name$ >>, <:expr< $lid:"gcata"$ >> in
                               <:expr< $name$ . $gcata$ >>
      in
      let g     = name_generator (map (fun (_, n, _) -> n) d) in
      let trans = g#generate "t"   in
      let ext   = g#generate "ext" in
      let farg  = 
        let module M = Map.Make (String) in
        let m = ref M.empty in
        (fun a -> 
           let p = "f" ^ a in
           try M.find p !m with
             Not_found -> 
               let n = g#generate p in
               m := M.add p n !m;
               n
        ) 
      in
      let subj = g#generate "s"   in
      let acc  = g#generate "acc" in
      let defs =
        map 
          (fun (args, name, descr) -> 
             let of_lid name = <:expr< $lid:name$ >> in
             let current    = name in
             let extensible, remove_bound_var, is_bound_var, bound_var = 
               match descr with 
               | `Poly (`More x, _) -> true, filter (fun s -> s <> x), (fun s -> s = x), Some x
               | _ -> false, (fun x -> x), (fun _ -> false), None
             in
             let polyvar     = match descr with `Poly _ -> true | _ -> false in
             let orig_args   = args                     in
             let args        = remove_bound_var args    in
             let generator   = name_generator args      in
             let targs       = map (fun arg -> arg, generator#generate ("t" ^ arg)) args in
             let img name    = assoc name targs         in
             let inh         = generator#generate "inh" in
             let syn         = generator#generate "syn" in
             let class_targs = (match bound_var with None -> [] | Some x -> [x]) @ 
                               (flatten (map (fun (x, y) -> [x; y]) targs)) @ 
                               [inh; syn]              in

             let metargs     = (if extensible then [ext] else ["_"]) @ [trans] @ (map farg args) in
             let args        = metargs @ [acc; subj] in
             let get_type_handler, get_local_defs =
               let context = ref [] in
               (fun (args, qname) as typ ->
                  let args = match qname with [name] when name = current -> remove_bound_var args | _ -> args in
                  let name = 
                    try fst (assoc typ !context) with
                      Not_found ->
                        let compound_name = String.concat "_" (args @ qname) in
                        let base_gcata, ext = 
                          match qname with
                          | [name]      -> get_cata name, if name = current && extensible then [<:expr< $lid:ext$ >>] else [] 
                          | name::names -> 
                              let base = 
                                fold_left (fun q name -> 
                                             let name = if Char.lowercase name.[0] = name.[0] 
                                                        then <:expr< $lid:name$ >>
                                                        else <:expr< $uid:name$ >>
                                             in
                                             <:expr< $q$ . $name$ >>   
                                          ) 
                                          <:expr< $uid:name$ >> 
                                          names
                              in
                              let fname = <:expr< $uid:"Generic"$ >> in
                              let fname = <:expr< $fname$ . $lid:"gcata"$ >> in
                              <:expr< $base$ . $fname$ >>, []
                        in
                        let impl =
                          (
                           <:patt< $lid:compound_name$ >>, 
                           make_fun 
                             (fun a -> <:patt< $lid:a$ >>)
                             [acc; subj] 
                             (make_call id base_gcata (ext @ map of_lid ([trans] @ (map (fun a -> if is_bound_var a then "self" else farg a) args) @ [acc; subj])))
                          )
                        in
                        let name = <:expr< $lid:compound_name$ >> in
                        context := (typ, (name, impl)) :: !context;
                        name
                  in
                  name
               ),
               (fun () ->
                  map (fun (_, (_, x)) -> x) !context
               )
             in
             let match_cases =
               map 
                 (fun (cname, cargs) -> 
                    let args, _ = fold_right (fun arg (acc, n) -> (sprintf "p%d" n) :: acc, n+1) cargs ([], 1) in
                    let args    = rev args in
                    let patt    =
                      fold_left 
                        (fun p id -> let pid = <:patt< $lid:id$ >> in <:patt< $p$ $pid$ >>)
                        (if polyvar then <:patt< ` $cname$ >> else <:patt< $uid:cname$ >>) 
                        args
                    in
                    let met_name = cmethod cname in
                    let met_sig  = 
                      let make_a x y z = 
                        let g  = <:ctyp< $uid:"Generic"$ >> in
                        let a  = <:ctyp< $lid:"a"$ >> in
                        let ga = <:ctyp< $g$ . $a$ >> in
                        let ga = <:ctyp< $ga$ $x$ >> in
                        let ga = <:ctyp< $ga$ $y$ >> in
                        <:ctyp< $ga$ $z$ >>
                      in
                      let make_typ = function
                      | `Protected   t    -> t
                      | `Variable    name -> make_a <:ctyp< ' $inh$ >> <:ctyp< ' $name$ >> <:ctyp< ' $img name$ >>
                      | `Processing (targs, qname) ->   
                           let typ =
                             let qtype =
                               match rev qname with
                               | name::qname -> 
                                  fold_right 
                                    (fun a acc -> let t = <:ctyp< $uid:a$ >> in <:ctyp< $t$ . $acc$ >>) 
                                    qname 
                                    <:ctyp< $lid:name$ >>
                             in
                             fold_left 
                               (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) 
                               qtype 
                               targs
                           in
                           make_a <:ctyp< ' $inh$ >> typ <:ctyp< ' $syn$ >>
                      in
                      let typs = [<:ctyp< ' $inh$ >>; make_typ (`Processing (orig_args, [name]))] @ (map make_typ cargs) in
                      fold_right (fun t s -> <:ctyp< $t$ -> $s$ >> ) typs <:ctyp< ' $syn$ >>
                    in
                    let expr =
                      let obj      = <:expr< $lid:trans$ >>        in
                      let met      = <:expr< $obj$ # $met_name$ >> in
                      let garg f x =
                        let g = <:expr< $uid:"Generic"$ >> in
                        let m = <:expr< $lid:"make"$ >> in
                        let gm = <:expr< $g$ . $m$ >> in
                        make_call id gm [f; x]
                      in
                      make_call id 
                        met 
                        (<:expr< $lid:acc$  >> :: 
                         (garg <:expr< $lid:"self"$ >> <:expr< $lid:subj$ >>) :: 
                         (map (fun (typ, x) -> 
                                 match typ with
                                 | `Protected _   -> <:expr< $lid:x$ >>
                                 | `Variable name -> garg <:expr< $lid:farg name$ >> <:expr< $lid:x$ >>
                                 | `Processing t   -> 
                                     let name = get_type_handler t in 
                                     garg name <:expr< $lid:x$ >>
                              ) 
                              (combine cargs args)
                         )
                        )
                    in
                    (patt, VaVal None, expr), [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>]
                 ) 
                 (match descr with `Vari cons | `Poly (_, cons) -> cons)
             in
             let match_cases = 
               if extensible then match_cases @ [(<:patt< $lid:others$ >>, 
                                                  VaVal None,
                                                  make_call of_lid <:expr< $lid:ext$ >> ["self"; acc; others]
                                                 ),
                                                 []
                                                ]
                             else match_cases
             in
             let subj = <:expr< $lid:subj$ >> in 
             let local_defs_and_then expr =
               let local_defs =
                  get_local_defs () @
                  if extensible then [<:patt< $lid:"self"$ >>, make_call of_lid <:expr< $lid:cata current$ >> metargs]
                                else []
               in
               match local_defs with
               | [] -> expr
               | _  -> <:expr< let $list:local_defs$ in $expr$ >>
             in
             let cases, methods = split match_cases in
             let methods = flatten methods in
             let class_expr = <:class_expr< object $list:methods$ end >> in
             let class_info = { 
                ciLoc = loc;
                ciVir = Ploc.VaVal true;
                ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
                ciNam = Ploc.VaVal (name ^ "_t");
                ciExp = class_expr;
               } 
             in
             let class_def = <:str_item< class $list:[class_info]$ >> in
             (<:patt< $lid:cata name$ >>, 
              (make_fun (fun a -> <:patt< $lid:a$ >>) args (local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>))
             ),
             class_def
          ) 
          d
      in
      let generic_cata = 
        let g = <:patt< $uid:"Generic"$ >> in
        let c = <:patt< $lid:"gcata"$ >> in
        <:patt< $g$ . $c$ >>
      in
      let generic_cata_ext = 
        let g = <:patt< $uid:"Generic"$ >> in
        let c = <:patt< $lid:"gcata_ext"$ >> in
        <:patt< $g$ . $c$ >>
      in
      let pnames, tnames = 
        split (
          map (fun (args, name, descr) -> 
                 let p = snd (fold_left (fun (i, acc) _ -> i+1, (sprintf "p%d" i)::acc) (0, []) (["t"; "acc"; "s"] @ args)) in
                 let pe = [
                   generic_cata_ext, <:expr< $lid:cata name$ >>; 
                   generic_cata, let ext, p = 
                                   match descr with 
                                   | `Poly (`More _, _) -> 
                                        let p = tl p in
                                        let px = <:patt< $lid:"x"$ >> in
                                        let tx = 
                                          fold_left 
                                            (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) 
                                            <:ctyp< $lid:closed name$ >> 
                                            args
                                        in                                        
                                        make_fun id 
                                          [<:patt< $lid:"f"$ >>; <:patt< $lid:"acc"$ >>; <:patt< ( $px$ : $tx$ ) >>] 
                                          (make_call id 
                                             <:expr< $lid:"f"$ >> 
                                             [<:expr< $lid:"acc"$ >>; <:expr< $lid:"x"$ >>]
                                          ), p

                                   | _ -> 
                                        let g = <:expr< $uid:"Generic"$ >> in
                                        let a = <:expr< $lid:"apply"$ >> in
                                        <:expr< $g$ . $a$ >>, p
                                 in
                                 let args = ext :: map (fun arg -> <:expr< $lid:arg$ >>) p in
                                 let cata = <:expr< $lid:cata name$ >> in
                                 make_fun (fun a -> <:patt< $lid:a$ >>) p (make_call id cata args)
                 ] 
                 in 
                 <:patt< $lid:name$ >>, <:expr< { $list:pe$ } >>
              ) 
              d
        ) 
      in
      let tuple = <:patt< ( $list:pnames$ ) >> in
      let tup = <:expr< ( $list:tnames$ ) >> in 
      let defs, class_defs = split defs in
      let def = <:expr< let rec $list:defs$ in $tup$ >> in
      let cata_def = <:str_item< value $list:[tuple, def]$ >> in
      let type_def = <:str_item< type $list:t$ >> in
      <:str_item< declare $list:[type_def; cata_def] @ class_defs$ end >> ]
  ];

  t_decl: [
    [ a=fargs; n=LIDENT; "="; t=rhs -> 
      let def, cons = fst t in
      let default_version =
      {
       tdNam = VaVal (loc, VaVal n);
       tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
       tdPrv = VaVal false;
       tdDef = def; 
       tdCon = VaVal cons
      }
      in      
      let t = snd t in
      let descriptor, open_version =
        match t with
        | `Poly (`More b, d) -> 
           (match a with 
           | f::_ when f <> b -> invalid_arg (sprintf "type argument \"%s\" should be listed first in type \"%s\" definition." b n) 
           | [] -> invalid_arg (sprintf "type \"%s\" should atleast have type argument \"%s\"." n b) 
           | _  -> (a, n, t)
           ),
           (let lcons =
              map 
                (fun (constr, args) -> 
                   match args with
                   | [] -> <:poly_variant< `$constr$ >>
                   | _  ->
                      let args = 
                        map 
                          (function 
                           | `Protected t -> 
                               let rec replace_t = 
                                 let replace_pv lpv = 
                                   map 
                                     (function <:poly_variant< `$name$ of $flag:f$ $list:args$ >> -> 
                                        let args = map replace_t args in
                                        <:poly_variant< `$name$ of $flag:f$ $list:args$ >>
                                     ) 
                                     lpv 
                                 in
                                 function 
                                 | <:ctyp< $t1$ as $t2$ >> -> <:ctyp< $replace_t t1$ as $replace_t t2$ >>           
                                 | <:ctyp< $t1$ $t2$ >> -> 
                                     (match t1 with
                                      | <:ctyp< $lid:s$ >> as typ when s = n ->
                                         let rec inner args t =
                                           match args, t with
                                           | [arg], <:ctyp< ' $b$ >> -> if arg = b then <:ctyp< ' $hd a$ >> else typ
                                           | arg::args, <:ctyp< $t1$ $t2$ >> ->
                                              (match t1 with 
                                               | <:ctyp< ' $b$ >> when arg = b -> inner args t2
                                               | _ -> typ
                                              )
                                           | _ -> typ
                                         in
                                         inner a t2

                                      | _ -> <:ctyp< $replace_t t1$ $replace_t t2$ >>
                                     )
                                 | <:ctyp< $t1$ -> $t2$ >> -> <:ctyp< $replace_t t1$ -> $replace_t t2$ >>
                                 | <:ctyp< ~$s$: $t$ >> -> <:ctyp< ~$s$: $replace_t t$ >>
                                 | <:ctyp< $t1$ == private $t2$ >> -> <:ctyp< $replace_t t1$ == private $replace_t t2$ >>
                                 | <:ctyp< $t1$ == $t2$ >> -> <:ctyp< $replace_t t1$ == $replace_t t2$ >>
                                 | <:ctyp< ?$s$: $t$ >> -> <:ctyp< ?$s$: $replace_t t$ >>
                                 | <:ctyp< ( $list:lt$ ) >> -> <:ctyp< ( $list:map replace_t lt$ ) >> 
                                 | <:ctyp< [ > $list:lpv$ ] >> -> <:ctyp< [ > $list:replace_pv lpv$ ] >>
                                 | <:ctyp< [ < $list:lpv$ ] >> -> <:ctyp< [ < $list:replace_pv lpv$ ] >>
                                 | <:ctyp< < $list:lst$ > >> -> <:ctyp< < $list:map (fun (s, t) -> s, replace_t t) lst$ > >>
                                 | <:ctyp< < $list:lst$ .. > >> -> <:ctyp< < $list:map (fun (s, t) -> s, replace_t t) lst$ .. > >>  
                                 | typ -> typ
                               in 
                               replace_t t

                           | `Processing (targs, [name]) when name = n && targs = a -> <:ctyp< ' $hd a$ >>
                           | `Processing (targs, qname) ->   
                               let qtype =
                                 match rev qname with
                                 | name::qname -> 
                                    fold_right 
                                      (fun a acc -> let t = <:ctyp< $uid:a$ >> in <:ctyp< $t$ . $acc$ >>) 
                                      qname 
                                      <:ctyp< $lid:name$ >>
                               in
                               fold_left 
                                 (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) 
                                 qtype 
                                 targs
                           | `Variable a -> <:ctyp< ' $a$ >>
                          )
                          args
                      in
                      let args = [<:ctyp< ($list:args$) >>] in
                      <:poly_variant< `$constr$ of $flag:false$ $list:args$ >>
                ) 
                d
            in
            [{
              tdNam = VaVal (loc, VaVal (closed n));
              tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
              tdPrv = VaVal false;
              tdDef = <:ctyp< [ = $list:lcons$ ] >>; 
              tdCon = VaVal []            
             }]
           )
        | _ -> (a, n, t), []
      in
      default_version::open_version, descriptor
    ]
  ];

  rhs: [[ vari ] | [ poly ]];

  vari: [
    [ OPT "|"; vari_cons=LIST1 vari_con SEP "|" -> 
        let x, y = split vari_cons in
        (<:ctyp< [ $list:x$ ] >>, []), `Vari y
    ]
  ];

  vari_con: [
    [ c=UIDENT; a=con_args -> (loc, VaVal c, fst a, None), (c, snd a) ]
  ];

  poly: [
    [ "["; body=poly_body; "]" -> 
        let lcons, y = body in
        (<:ctyp< [ = $list:lcons$ ] >>, []), `Poly (`Equal, y) 
    ] |
    [ a=targ; "as"; "["; ">"; body=poly_body; "]" ->
        let lcons, y = body in
        (<:ctyp< ' $a$ >>, [<:ctyp< ' $a$ >>, <:ctyp< [ > $list:lcons$ ] >>]), `Poly (`More a, y)
    ]
  ];

  poly_body: [
    [ OPT "|"; poly_cons=LIST1 poly_con SEP "|" ->
        let x, y = split poly_cons in
        let lcons = map (fun (loc, name, args, _) ->                                  
	    	    	     	 if length args = 0 
                                 then <:poly_variant< `$name$ >>         
                                 else                          
                                   let args = [<:ctyp< ($list:args$) >>] in
                                   <:poly_variant< `$name$ of $flag:false$ $list:args$ >> 
                              ) x
        in
        lcons, y
    ]
  ];

  poly_con: [
    [ "`"; c=UIDENT; a=con_args -> (loc, c, get_val (fst a), None), (c, snd a) ]
  ];

  con_args: [
    [ "of"; a=LIST1 typ SEP "*" -> let x, y = split a in VaVal x, y ] |
    [ -> VaVal [], [] ]
  ];

  typ: [ 
    [ "["; t=ctyp; "]" -> t, `Protected t ] |
    [ c_typ ]
  ];

  c_typ: [
    [ a=targ; q=OPT qname -> 
       let at = <:ctyp< ' $a$ >> in
       match q with 
       | Some q -> <:ctyp< $fst q$ $at$ >>, `Processing ([a], snd q)
       | None -> <:ctyp< ' $a$ >>, `Variable a
    ] | 
    [ "("; a=LIST1 targ SEP ","; ")"; q=qname -> 
       fold_left (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) (fst q) a, 
       `Processing (a, snd q) 
    ] |
    [ q=qname -> fst q, `Processing ([], snd q) ]
  ];

  qname: [
    [ x=LIDENT -> <:ctyp< $lid:x$ >>, [x] ] |
    [ q=UIDENT; "."; n=qname -> 
       let t = <:ctyp< $uid:q$ >> in 
       <:ctyp< $t$ . $fst n$ >>, q :: snd n
    ]
  ];

  fargs: [
    [ a=targ -> [a] ] |
    [ "("; a=LIST1 targ SEP ","; ")" -> a ] |
    [ -> [] ]
  ];

  targ: [
    [ "'"; a=LIDENT -> a ]
  ];
  
END;
