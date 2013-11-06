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
open Plugin

exception Generic_extension of string

let get_val (VaVal x) = x 

module S = Set.Make (String)

let split3 l = 
  List.fold_right 
    (fun (a, b, c) (x, y, z) -> a::x, b::y, c::z) l ([], [], []) 

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
    let s = fold_left (fun s ((_, n, _), _) -> S.add n s) S.empty d in
    fun name -> 
      if S.mem name s then <:expr< $lid:cata name$ >> 
                      else let name, gcata = <:expr< $lid:name$ >>, <:expr< $lid:"gcata"$ >> in
                           <:expr< $name$ . $gcata$ >>
  in
  let cluster_names = 
    fold_left 
      (fun acc ((_, n, d), _) -> 
         let acc = n::acc in
         match d with
         | `Poly (_, comps) ->
             fold_left 
               (fun acc t ->
                  match t with
                  | `Specific (_, [n]) -> n::acc
                  | _ -> acc
               ) 
               acc 
               comps
         | _ -> acc
      ) 
      [] 
      d 
  in
  let g = name_generator cluster_names in
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
      (fun ((args, name, descr), deriving) ->     
         Plugin.load_plugins deriving;
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
         let proper_args = flatten (map (fun (x, y) -> [x; y]) targs) @ [inh; syn] in
         let class_targs = (match bound_var with None -> [] | Some x -> [x]) @ proper_args in
         let p_descriptor = {
           Plugin.is_polyvar = polyvar;
           Plugin.is_open    = (match bound_var with Some b -> `Yes b | _ -> `No);
           Plugin.type_args  = args;
           Plugin.name       = current;
           Plugin.default    = { 
             Plugin.inh         = <:ctyp< ' $inh$ >>;
             Plugin.syn         = <:ctyp< ' $syn$ >>;
             Plugin.proper_args = proper_args;
             Plugin.arg_img     = (fun a -> <:ctyp< ' $img a$ >>);
           }
         } 
         in
         let derived = map (fun name -> let Some p = Plugin.get name in name, p loc p_descriptor) deriving in
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
         let catype  =
           let gt = 
             let x = <:ctyp< $uid:"GT"$ >> in
             let y = <:ctyp< $lid:"t"$  >> in
             <:ctyp< $x$ . $y$ >> 
           in
           let ft subj = 
             let x = <:ctyp< ' $syn$ >> in
             let y = <:ctyp< $subj$ -> $x$ >> in
             let z = <:ctyp< ' $inh$ >> in
             <:ctyp< $z$ -> $y$ >> 
           in             
           let trt subj =            
             fold_left 
               (fun t ti -> <:ctyp< $t$ $ti$ >>) 
               <:ctyp< # $list:[class_t name]$ >>
               (subj :: (flatten (map (fun a -> [<:ctyp< ' $a$ >>; <:ctyp< ' $img a$ >>]) args)) @ 
                        [<:ctyp< ' $inh$ >>; <:ctyp< ' $syn$ >>]
               )  
           in
           let extt = <:ctyp< $ft orig_typ$ -> $ft orig_typ$ >> in
           let closed_typ =
             if extensible 
             then
               let Some bound_var = bound_var in 
               let b = <:ctyp< ' $bound_var$ >> in
               <:ctyp< $closed_typ$ as $b$ >>
             else closed_typ
           in
           let ft, ft_ext    = ft closed_typ, ft orig_typ in
           let cata_type     = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt closed_typ]) ft in
           let cata_ext_type = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt orig_typ; extt]) ft_ext in
           let x = <:ctyp< $gt$ $cata_type$ >> in
           <:ctyp< $x$ $cata_ext_type$ >> 
         in
         let metargs = (map farg args) @ [trans; ext] in
         let args = metargs @ [acc; subj] in
         match descr with
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
                        | [name]      -> get_cata name, [<:expr< $lid:ext$ >>]
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
                            let fname = <:expr< $uid:"GT"$ >> in
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
	   let add_derived_member, get_derived_classes =
             let module M    = Map.Make (String) in
             let get k m     = try M.find k m with Not_found -> [] in
             let mdef, mdecl = ref M.empty, ref M.empty in
             let addp_g      = name_generator cluster_names in
             let addp        = ref [] in
             (*let register_p  = function
             | `Transform name     ->
             | `Trait (typ, trait) ->
             in*)
             (fun case (trait, (prop, p_func) as dprop) ->
                let Some p = Plugin.get trait in
                let prev_def, prev_decl = get trait !mdef, get trait !mdecl in
                let def =
                  match case with
                  | `Con (cname, cargs) ->
                     let g = name_generator cluster_names in                      
                     let args = fst (fold_right (fun _ (acc, i) -> (g#generate (sprintf "p%d" i))::acc, i+1) cargs ([], 0)) in
                     let constr = {
                       Plugin.constr = cname;
                       Plugin.acc    = g#generate "acc";
                       Plugin.subj   = g#generate "subj";
                       Plugin.args   = combine args cargs;
                     }
                     in
                     let env = {
                       Plugin.get_name      = (fun s -> g#generate s); 
                       Plugin.get_trait     = (fun s -> invalid_arg ""); 
                       Plugin.get_transform = (fun s -> invalid_arg "")
                     } 
                     in
                     let m_def = 
                       let name = cmethod cname in
                       let body = make_fun (fun a -> <:patt< $lid:a$ >>) ([constr.Plugin.acc; constr.Plugin.subj] @ args) (p_func env constr) in
                       <:class_str_item< method $lid:name$ = $body$ >>
                     in
                     m_def

                  | `Specific (b::args, qname) -> 
                     let qname, name = 
                       let n::t = rev qname in
                       rev ((trait_t n trait) :: t), n
                     in
                     let descr = {
                       Plugin.is_polyvar = true;
                       Plugin.is_open    = `Yes b;
                       Plugin.type_args  = args;
                       Plugin.name       = name;
                       Plugin.default    = prop;
                     }
                     in
                     let i_def, i_decl = Plugin.generate_inherit false loc qname descr (p loc descr) in
                     i_def
                in
                mdef := M.add trait (def::prev_def) !mdef
             ),
             (fun (trait, p) -> 
	        let m_defs = get trait !mdef in 
                let i_def, i_decl = Plugin.generate_inherit true loc [class_t current] p_descriptor p in
                let ce = <:class_expr< object $list:i_def::m_defs$ end >> in
                let ct = <:class_type< object $list:[]$ end >> in
                Plugin.generate_classes loc trait p_descriptor p (ce, ct)                
	     )
           in
           let match_cases =
             map 
               (function 
                | `Con (cname, cargs) as case -> 
                    iter (add_derived_member case) derived;
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
                        let g  = <:ctyp< $uid:"GT"$ >> in
                        let a  = <:ctyp< $lid:"a"$  >> in
                        let ga = <:ctyp< $g$ . $a$  >> in
                        let ga = <:ctyp< $ga$ $x$   >> in
                        let ga = <:ctyp< $ga$ $y$   >> in
                        let ga = <:ctyp< $ga$ $z$   >> in
                        <:ctyp< $ga$ $tpt$ >>                           
                      in
                      let make_typ = function
                      | `Generic   t    -> t
                      | `Variable    name -> make_a <:ctyp< ' $inh$ >> <:ctyp< ' $name$ >> <:ctyp< ' $img name$ >>
                      | `Specific (targs, qname) ->   
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
                      let typs = [<:ctyp< ' $inh$ >>; make_typ (`Specific (orig_args, [name]))] @ (map make_typ cargs) in
                      fold_right (fun t s -> <:ctyp< $t$ -> $s$ >> ) typs <:ctyp< ' $syn$ >>
                    in
                    let expr =
                      let obj      = <:expr< $lid:trans$ >>        in
                      let met      = <:expr< $obj$ # $met_name$ >> in
                      let garg f x =
                        let g = <:expr< $uid:"GT"$ >> in
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
                                   | `Generic _   -> <:expr< $lid:x$ >>
                                   | `Variable name -> garg <:expr< $lid:farg name$ >> <:expr< $lid:x$ >>
                                   | `Specific t   -> 
                                       let name = get_type_handler t in 
                                       garg name <:expr< $lid:x$ >>
                                ) 
                                (combine cargs args)
                           )
                         )
                    in
                    (patt, VaVal None, expr),
                    [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
                    [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>]

                | `Specific (args, qname) as case -> 
                    iter (add_derived_member case) derived;
                    let h::tl = args in
                    let targs  = 
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
                    let tqname = 
                      let h::t = rev qname in
                      (rev t) @ [class_t h]
                    in
                    let targs = map (fun a -> <:ctyp< ' $a$ >>) targs in
                    let ce    = <:class_expr< [ $list:targs$ ] $list:tqname$ >> in
                    let ct    =
                      let h::t = tqname in
                      let ct   = 
                        fold_left 
                          (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
                          <:class_type< $id:h$ >>  
                          t
                      in
                      <:class_type< $ct$ [ $list:targs$ ] >>
                    in
                    let expr =
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
                      let generic = <:expr< $uid:"GT"$ >> in
                      let cata    = <:expr< $lid:"gcata_ext"$ >> in
                      let func    = <:expr< $typename$ . $generic$ >> in
                      let func    = <:expr< $func$ . $cata$ >> in
                      let ext     = 
                        make_fun id [<:patt< _ >>] <:expr< $lid:"self"$ >> 
                      in
                      make_call id func 
                        ((map (fun a -> <:expr< $lid:farg a$>>) args) @ 
                         [<:expr< $lid:trans$ >>; ext; <:expr< $lid:acc$ >>; <:expr< $lid:subj$ >>]
                        )
                    in
                    let patt =
                      let t::r  = rev qname in
                      let qname = rev (closed t :: r) in
                      let pvt  = <:patt< # $list:qname$ >> in
                      let subj = <:patt< $lid:subj$ >> in
                      <:patt< ( $pvt$ as $subj$ ) >>
                    in
                    (patt, VaVal None, expr),
                    [<:class_str_item< inherit $ce$ >>],
                    [<:class_sig_item< inherit $ct$ >>]

                | `Variable _ -> invalid_arg "should not happen"
               ) 
               (match descr with `Vari cons | `Poly (_, cons) -> cons)
           in
           let match_cases = 
             if extensible then match_cases @ [(<:patt< $lid:others$ >>, 
                                                 VaVal None,
                                                 make_call of_lid <:expr< $lid:ext$ >> ["self"; acc; others]
                                               ),
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
           let cases, methods, methods_sig = split3 match_cases in
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
           (<:patt< $lid:cata name$ >>, 
            (make_fun (fun a -> <:patt< $lid:a$ >>) args (local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>))
           ),
           <:sig_item< value $name$ : $catype$ >>,
           [class_def, class_decl] @ (map get_derived_classes derived)
      ) 
      d
  in
  let generic_cata = 
    let g = <:patt< $uid:"GT"$ >> in
    let c = <:patt< $lid:"gcata"$ >> in
    <:patt< $g$ . $c$ >>
  in
  let generic_cata_ext = 
    let g = <:patt< $uid:"GT"$ >> in
    let c = <:patt< $lid:"gcata_ext"$ >> in
    <:patt< $g$ . $c$ >>
  in
  let pnames, tnames = 
    split (
      map (fun ((args, name, descr), deriving) -> 
             let p = snd (fold_left (fun (i, acc) _ -> i+1, (sprintf "p%d" i)::acc) (0, []) (["t"; "acc"; "s"] @ args)) in
             let pe = [
               generic_cata_ext, <:expr< $lid:cata name$ >>; 
               generic_cata, (let ext, p = 
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
                                    make_fun 
                                      id 
                                      [<:patt< $lid:"f"$ >>; <:patt< $lid:"acc"$ >>; <:patt< ( $px$ : $tx$ ) >>] 
                                      (make_call id 
                                         <:expr< $lid:"f"$ >> 
                                         [<:expr< $lid:"acc"$ >>; <:expr< $lid:"x"$ >>]
                                      ), p

                                | _ -> 
                                    let g = <:expr< $uid:"GT"$ >> in
                                    let a = <:expr< $lid:"apply"$ >> in
                                    <:expr< $g$ . $a$ >>, p
                              in                                 
                              let args =
                                let x :: y :: a = rev (map (fun arg -> <:expr< $lid:arg$ >>) p) in
                                rev a @ [ext; y; x]
                              in
                              let cata = <:expr< $lid:cata name$ >> in
                              make_fun (fun a -> <:patt< $lid:a$ >>) p (make_call id cata args)
                             ) 
             ] 
             in 
             <:patt< $lid:name$ >>, <:expr< { $list:pe$ } >>
          ) 
          d
    ) 
  in
  let tuple = <:patt< ( $list:pnames$ ) >> in
  let tup = <:expr< ( $list:tnames$ ) >> in 
  let defs, decls, classes = split3 defs in
  let class_defs, class_decls = split (flatten classes) in
  let def = <:expr< let rec $list:defs$ in $tup$ >> in
  let cata_def  = <:str_item< value $list:[tuple, def]$ >> in
  let type_def  = <:str_item< type $list:t$ >> in
  let type_decl = <:sig_item< type $list:t$ >> in
  <:str_item< declare $list:[type_def; cata_def] @ class_defs$ end >>,
  <:sig_item< declare $list:[type_decl] @ class_decls @ decls$ end >> 
    
