
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

exception Generic_extension of string
     
let get_val (VaVal x) = x 

module S = Set.Make (String)

let split4 l = 
  List.fold_right 
    (fun (a, b, c, d) (x, y, z, t) -> a::x, b::y, c::z, d::t) l ([], [], [], []) 

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

let cata    name = name ^ "_gcata"
let others       = "others"
let cmethod c    = "m_" ^ c
let apply        = "apply"
let closed  name = name ^ "'"
let class_t name = name ^ "_t"

let rec replace_t loc a n typ =
  let replace_t = replace_t loc a n in 
  let replace_pv lpv = 
    map 
      (function <:poly_variant< `$name$ of $flag:f$ $list:args$ >> -> 
         let args = map replace_t args in
         <:poly_variant< `$name$ of $flag:f$ $list:args$ >>
      ) 
      lpv 
  in
  match typ with
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

let generate t loc =
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
  let g = 
    let names = 
      fold_left 
        (fun acc (_, n, d) -> 
           let acc = n::acc in
           match d with
           | `Sumi (_, _, types) ->
               fold_left 
                 (fun acc t ->
                    match t with
                    | `Processing (_, [n]) -> n::acc
                    | _ -> acc
                 ) 
                 acc 
                 types
           | _ -> acc
        ) 
        [] 
        d 
    in
    name_generator names
  in
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
         let orig_typ, closed_typ =
           let make t args =
             fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >> ) t args
           in
           make <:ctyp< $lid:name$ >> args,
           make <:ctyp< $lid:closed name$ >> args           
         in
         let current     = name in
         let extensible, remove_bound_var, is_bound_var, bound_var = 
           match descr with 
           | `Sumi (x, _, _) | `Poly (`More x, _) -> true, filter (fun s -> s <> x), (fun s -> s = x), Some x
           | _ -> false, (fun x -> x), (fun _ -> false), None
         in
         let polyvar     = match descr with `Poly _ | `Sumi _ -> true | _ -> false in
         let orig_args   = args                     in
         let args        = remove_bound_var args    in
         let generator   = name_generator args      in
         let targs       = map (fun arg -> arg, generator#generate ("t" ^ arg)) args in
         let img name    = assoc name targs         in
         let inh         = generator#generate "inh" in
         let syn         = generator#generate "syn" in
         let class_targs = (match bound_var with None -> [] | Some x -> [x]) @ 
                           (flatten (map (fun (x, y) -> [x; y]) targs)) @ 
                           [inh; syn]              
         in
         let tpo_name = generator#generate "tpo" in
         let tpo =
           let methods = 
             map (fun a -> let e = <:expr< $lid:farg a$ >> in <:class_str_item< method $lid:a$ = $e$ >>) args 
           in
           <:expr< object $list:methods$ end >>
         in
         let tpf =
           map (fun a -> 
                  let inh, e, te = <:ctyp< ' $inh$ >>, <:ctyp< ' $a$ >>, <:ctyp< ' $img a$ >> in 
                  fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >>) [inh; e] te 
               )
               args
         in
         let tpt     = <:ctyp< < $list:combine args tpf$ > >> in
         let metargs = (map farg args) @ [trans; ext] in
         let args = metargs @ [acc; subj] in
         match descr with
         | `Sumi (_, _, typs) -> 
           let inherits =
             map (function 
                  | `Processing (args, qname) ->
                      let h::tl = args in
                      let args  = 
                        h ::
                        (flatten 
                          (map 
                             (fun a -> 
                                try [a; img a] with 
                                | Not_found -> 
                                    Ploc.raise loc (Generic_extension (sprintf "unbound type variable '%s" a))
                             ) 
                             tl
                          )
                        ) @ 
                        [inh; syn] 
                      in
                      let qname = 
                        let h::t = rev qname in
                        (rev t) @ [class_t h]
                      in
                      let args  = map (fun a -> <:ctyp< ' $a$ >>) args in
                      let ce    = <:class_expr< [ $list:args$ ] $list:qname$ >> in
                      let ct    =
                        let h::t = qname in
                        let ct   = 
                          fold_left 
                            (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
                            <:class_type< $id:h$ >>  
                            t
                        in
                        <:class_type< $ct$ [ $list:args$ ] >>
                      in
                      <:class_str_item< inherit $ce$ >>,
                      <:class_sig_item< inherit $ct$ >>

                  | _ -> invalid_arg "should not happen"
                 ) 
                 typs
           in
           let inherits, inherits_t = split inherits in
           let class_expr = <:class_expr< object $list:inherits$   end >> in
           let class_type = <:class_type< object $list:inherits_t$ end >> in
           let class_info c = { 
              ciLoc = loc;
              ciVir = Ploc.VaVal true;
              ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
              ciNam = Ploc.VaVal (class_t name);
              ciExp = c
             } 
           in
           let class_def  = <:str_item< class $list:[class_info class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info class_type]$ >> in
           let sum_body  =
             let summand = function
             | `Variable _ -> invalid_arg "should not happen"                 
             | `Processing (args, qname) -> 
                let _::t = args  in
                let args = rev t in
                let typename =
                  match qname with
                  | [n]  -> <:expr< $lid:n$ >>
                  | h::t -> 
                     let n::t = rev t in
                     let n = <:expr< $lid:n$ >> in
                     let q = 
                       fold_left 
                         (fun q n -> let n = <:expr< $uid:n$ >> in <:expr< $q$ . $n$ >>) 
                         <:expr< $uid:h$ >> 
                         (rev t) 
                     in
                     <:expr< $q$ . $n$ >>
                in
                let generic = <:expr< $uid:"Generic"$ >> in
                let cata    = <:expr< $lid:"gcata_ext"$ >> in
                let func    = <:expr< $typename$ . $generic$ >> in
                let func    = <:expr< $func$ . $cata$ >> in
                make_call of_lid func ((map farg args) @ [trans])
             in
             let h::t    = typs in
             let generic = <:expr< $uid:"Generic"$ >> in
             let sum     = <:expr< $lid:"sum"$ >> in
             let gsum    = <:expr< $generic$ . $sum$ >> in
             let sumcata = fold_left (fun l r -> make_call id gsum [l; summand r]) (summand h) t in
             make_call of_lid sumcata [ext; acc; subj]
           in
           let catype = <:ctyp< $lid:"int"$ >> in
           (<:patt< $lid:cata name$ >>, 
            (make_fun (fun a -> <:patt< $lid:a$ >>) args sum_body)
           ),
           <:sig_item< value $name$ : $catype$ >>,
           class_def,
	   class_decl
                
         | (`Poly _ | `Vari _) as descr -> 
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
                           (make_call 
                              id 
                              base_gcata 
                              (map of_lid ((map (fun a -> if is_bound_var a then "self" else farg a) args) @ [trans]) @ 
                               ext @ 
                               map of_lid [acc; subj]
                              )
                           )
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
                      let a  = <:ctyp< $lid:"a"$  >> in
                      let ga = <:ctyp< $g$ . $a$  >> in
                      let ga = <:ctyp< $ga$ $x$   >> in
                      let ga = <:ctyp< $ga$ $y$   >> in
                      let ga = <:ctyp< $ga$ $z$   >> in
                      <:ctyp< $ga$ $tpt$ >>                           
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
                                  <:ctyp< $lid: name$ >>
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
                      make_call id gm [f; x; <:expr< $lid:tpo_name$ >>]
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
                  (patt, VaVal None, expr), 
                  [met_name, met_sig],
                  [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
                  [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>]
               ) 
               (match descr with `Vari cons | `Poly (_, cons) -> cons)
           in
           let match_cases = 
             if extensible then match_cases @ [(<:patt< $lid:others$ >>, 
                                                VaVal None,
                                                make_call of_lid <:expr< $lid:ext$ >> ["self"; acc; others]
                                               ),
                                               [],
                                               [],
                                               []
                                              ]
                           else match_cases
           in
           let subj = <:expr< $lid:subj$ >> in 
           let local_defs_and_then expr =
             let local_defs =
                get_local_defs () @
                [<:patt< $lid:"self"$ >>  , make_call of_lid <:expr< $lid:cata current$ >> metargs;
                 <:patt< $lid:tpo_name$ >>, tpo
                ]                                                   
             in
             match local_defs with
             | [] -> expr
             | _  -> <:expr< let $list:local_defs$ in $expr$ >>
           in
           let cases, objt_methods, methods, methods_sig = split4 match_cases in
           let objt_methods = flatten objt_methods in
           let methods      = flatten methods      in
           let methods_sig  = flatten methods_sig  in
           let class_expr   = <:class_expr< object $list:methods$     end >> in
           let class_type   = <:class_type< object $list:methods_sig$ end >> in
           let class_info c = { 
              ciLoc = loc;
              ciVir = Ploc.VaVal true;
              ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
              ciNam = Ploc.VaVal (class_t name);
              ciExp = c;
             } 
           in
           let class_def  = <:str_item< class $list:[class_info class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info class_type]$ >> in 
           let catype = 
             let gt = 
               let x = <:ctyp< $uid:"Generic"$ >> in
               let y = <:ctyp< $lid:"t"$  >> in
               <:ctyp< $x$ . $y$ >> 
             in
             let ft subj = 
               let x = <:ctyp< ' $syn$ >> in
               let y = <:ctyp< $subj$ -> $x$ >> in
               let z = <:ctyp< ' $inh$ >> in
               <:ctyp< $z$ -> $y$ >> 
             in             
             let trt      = <:ctyp< < $list:objt_methods$ .. > >> in
             let extt     = <:ctyp< $ft orig_typ$ -> $ft orig_typ$ >> in
             let orig_typ =
               if extensible 
               then
                 let Some bound_var = bound_var in 
                 let b = <:ctyp< ' $bound_var$ >> in
                 <:ctyp< $orig_typ$ as $b$ >>
               else orig_typ
             in
             let ft, ft_ext    = ft closed_typ, ft orig_typ in
             let cata_type     = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt]) ft in
             let cata_ext_type = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt; extt]) ft_ext in
             let x = <:ctyp< $gt$ $cata_type$ >> in
             <:ctyp< $x$ $cata_ext_type$ >>              
           in
           (<:patt< $lid:cata name$ >>, 
            (make_fun (fun a -> <:patt< $lid:a$ >>) args (local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>))
           ),
           <:sig_item< value $name$ : $catype$ >>,
           class_def,
           class_decl
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
                               | `Poly (`More _, _) | `Sumi _ -> 
                                    let p = tl p in
                                    let px = <:patt< $lid:"x"$ >> in
                                    let tx = 
                                      fold_left 
                                        (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) 
                                        <:ctyp< $lid:closed name$ >> 
                                        args
                                    in                                        
                                    make_fun 
                                      id 
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
                             let args =
                                let x :: y :: a = rev (map (fun arg -> <:expr< $lid:arg$ >>) p) in
                                rev a @ [ext; y; x]
                             in
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
  let defs, decls, class_defs, class_decls = split4 defs in
  let def = <:expr< let rec $list:defs$ in $tup$ >> in
  let cata_def  = <:str_item< value $list:[tuple, def]$ >> in
  let type_def  = <:str_item< type $list:t$ >> in
  let type_decl = <:sig_item< type $list:t$ >> in
  <:str_item< declare $list:[type_def; cata_def] @ class_defs$ end >>,
  <:sig_item< declare $list:[type_decl] @ decls @ class_decls$ end >> 
    
EXTEND
  GLOBAL: sig_item str_item ctyp class_expr class_longident; 

  ctyp: [
    [ "@"; t=ctyp -> 
      let rec inner = function
      | <:ctyp< $q$ . $t$ >> -> <:ctyp< $q$ . $inner t$ >>
      | <:ctyp< $t$ $a$ >> -> <:ctyp< $inner t$ $a$ >>
      | <:ctyp< $lid:name$ >> -> <:ctyp< $lid:closed name$ >>
      | t -> Ploc.raise loc (Generic_extension "application or qualified name expected")
      in
      inner t
    ]
  ];
  
  class_longident: [
    [ "@"; ci=qname -> 
      let n::q = rev (snd ci) in
      rev ((class_t n)::q) 
    ]
  ];

  str_item: LEVEL "top" [
    [ "generic"; t=LIST1 t_decl SEP "and" -> fst (generate t loc) ]
  ];

  sig_item: LEVEL "top" [
    [ "generic"; t=LIST1 t_decl SEP "and" -> snd (generate t loc) ]
  ];

  t_decl: [
    [ a=fargs; n=LIDENT; "="; t=rhs -> 
      let def, cons = fst t in
      let t = snd t in
      let descriptor, types =
        match t with
        | `Sumi (var, lpv, _) -> 
           ignore (
             match a with
             | a::_ when a = var -> ()
             | _ -> Ploc.raise loc (Generic_extension (sprintf "sum type must be polymorphic with the first type variable '%s" var))
           );
           (a, n, t),
           [{
             tdNam = VaVal (loc, VaVal (closed n));
             tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
             tdPrv = VaVal false;
             tdDef = replace_t loc a n def; 
             tdCon = VaVal []            
            };
            {
             tdNam = VaVal (loc, VaVal n);
             tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
             tdPrv = VaVal false;
             tdDef = <:ctyp< ' $var$ >>; 
             tdCon = VaVal [<:ctyp< ' $var$ >>, <:ctyp< [ > $list:lpv$ ] >>]
            }]

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
                           | `Protected t -> replace_t loc a n t
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
                      let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in 
                      <:poly_variant< `$constr$ of $flag:false$ $list:args$ >>
                ) 
                d
            in
            [{
              tdNam = VaVal (loc, VaVal n);
              tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
              tdPrv = VaVal false;
              tdDef = def; 
              tdCon = VaVal cons
             };
             {
              tdNam = VaVal (loc, VaVal (closed n));
              tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
              tdPrv = VaVal false;
              tdDef = <:ctyp< [ = $list:lcons$ ] >>; 
              tdCon = VaVal []            
             }]
           )
        | _ -> (a, n, t), 
               [{tdNam = VaVal (loc, VaVal n);
                 tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
                 tdPrv = VaVal false;
                 tdDef = def; 
                 tdCon = VaVal cons
                };
                {tdNam = VaVal (loc, VaVal (closed n));
                 tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
                 tdPrv = VaVal false;
                 tdDef = fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) <:ctyp< $lid:n$>> a; 
                 tdCon = VaVal cons
                }
               ]
      in
      types, descriptor
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
    [ "["; ">"; body=poly_body; "]"; "as"; a=targ ->
        match body with
        | `TypeDef (lcons, y) ->
            (<:ctyp< ' $a$ >>, [<:ctyp< ' $a$ >>, <:ctyp< [ > $list:lcons$ ] >>]), `Poly (`More a, y)
        | `TypeSum s -> s
    ] |
    [ "["; body=poly_body; "]" -> 
        match body with
        | `TypeDef (lcons, y) ->
           (<:ctyp< [ = $list:lcons$ ] >>, []), `Poly (`Equal, y) 
        | `TypeSum s -> s
    ]    
  ];

  poly_body: [
    [ OPT "|"; poly_cons=LIST1 poly_con SEP "|" ->
        let x, y = split poly_cons in
        let lcons = map (fun (loc, name, args, _) ->                                  
	    	    	     	 if length args = 0 
                                 then <:poly_variant< `$name$ >>         
                                 else                          
                                   let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in
                                   <:poly_variant< `$name$ of $flag:false$ $list:args$ >> 
                              ) x
        in
        `TypeDef (lcons, y)
    ] |
    [ OPT "|"; typs=LIST1 c_typ SEP "|" ->
      let t, d = split typs in
      let Some a = 
        fold_left 
          (fun a d -> 
             match d with
             | `Variable x -> 
                 Ploc.raise loc (Generic_extension (sprintf "type variable ('%s) is not allowed in type sum" x))
             | `Processing ([], _) ->
                 Ploc.raise loc (Generic_extension "polymorphic type expected in type sum")
             | `Processing (b::_, _) ->
                 (match a with 
                 | None -> Some b
                 | Some a when a <> b -> 
                    Ploc.raise loc (Generic_extension (sprintf "type variable '%s should be the first parameter of all types this type sum" a))
                 | _ -> a
                 )
          ) 
          None 
          d
      in
      let t = 
        map 
          (fun t ->
             let rec replace = function
             | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $replace t1$ $t2$ >>
             | <:ctyp< $t1$ . $t2$ >> -> <:ctyp< $t1$ . $replace t2$ >>
             | <:ctyp< $lid:n$ >> -> <:ctyp< $lid:closed n$ >>
             | t -> t
             in 
             <:poly_variant< $replace t$ >>
          ) 
          t 
      in
      `TypeSum ((<:ctyp< [= $list:t$ ] >>, []), `Sumi (a, t, d))
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
    [ "["; t=c_typ; "]" -> 
      let t, d = t in
      t, (d :> [`Processing of string list * string list | `Variable of string | `Protected of ctyp])
    ] |
    [  t=ctyp LEVEL "apply" -> t, `Protected t ]
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
    [ q=LIST0 qualifier; x=LIDENT ->
      let x' = <:ctyp< $lid:x$ >> in
      match q with
      | []   -> x', [x]
      | h::t -> 
         let q' =
           fold_left 
             (fun q t -> 
                let t = <:ctyp< $uid:t$ >> in 
                <:ctyp< $q$ . $t$ >>
             ) 
             <:ctyp< $uid:h$ >> 
            t
         in
         <:ctyp< $q'$ . $x'$ >>, q@[x]
    ]
  ];

  qualifier: [
    [ x=UIDENT; "." -> x ]
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
