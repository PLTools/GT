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

let oops loc str = Ploc.raise loc (Generic_extension str)

let get_val (VaVal x) = x 

module S = Set.Make (String)

let map_last f l = 
  match rev l with
  | []    -> invalid_arg "empty list in map_last"
  | h::tl -> rev (f h :: tl)
  
let split3 l = 
  List.fold_right 
    (fun (a, b, c) (x, y, z) -> a::x, b::y, c::z) l ([], [], []) 

let split4 l = 
  List.fold_right 
    (fun (a, b, c, d) (x, y, z, t) -> a::x, b::y, c::z, d::t) l ([], [], [], []) 

let split5 l = 
  List.fold_right 
    (fun (a, b, c, d, e) (x, y, z, t, u) -> a::x, b::y, c::z, d::t, e::u) l ([], [], [], [], []) 

let name_generator list =
  let s = ref (fold_right S.add list S.empty) in
  object(self)
    method generate prompt =
      if S.mem prompt !s 
      then self#generate (prompt ^ "_")
      else (
        s := S.add prompt !s;
        prompt
      )
  end

let ctyp_of_qname loc (m::qname) =
  let id s = 
    if Char.lowercase s.[0] = s.[0] 
       then <:ctyp< $lid:s$ >> 
       else <:ctyp< $uid:s$ >> 
  in
  fold_left (fun q n -> <:ctyp< $q$ . $id n$ >> ) (id m) qname

let ctyp_of_instance loc args qname =
  fold_left (fun t a -> <:ctyp< $t$ $a$ >>) qname args

let generate t loc =
  let module H = Plugin.Helper (struct let loc = loc end) in

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
      if S.mem name s then H.E.id  (cata name)
                      else H.E.acc [H.E.id name; H.E.id "GT"; H.E.id "gcata"]
  in
  let cluster_names = 
    fold_left 
      (fun acc ((_, n, d), _) -> 
         let acc = n::acc in
         match d with
         | `Poly comps->
             fold_left 
               (fun acc t ->
                  match t with
                  | `Type (Instance (_, _, [n])) -> n::acc
                  | _ -> acc
               ) 
               acc 
               comps
         | _ -> acc
      ) 
      [] 
      d 
  in
  let g     = name_generator cluster_names in
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
  let subj         = g#generate "s"          in
  let acc          = g#generate "acc"        in
  let generic_cata = H.P.acc [H.P.id "GT"; H.P.id "gcata"] in
  let defs =
    map 
      (fun ((args, name, descr), deriving) ->     
         Plugin.load_plugins deriving;
         let current       = name in
         let polyvar       = match descr with `Poly _ -> true | _ -> false            in
         let orig_args     = args                                                     in
         let generator     = name_generator args                                      in
         let targs         = map (fun arg -> arg, generator#generate (targ arg)) args in
         let img name      = assoc name targs                                         in
         let inh           = generator#generate "inh"                                 in
         let syn           = generator#generate "syn"                                 in
         let proper_args   = flatten (map (fun (x, y) -> [x; y]) targs) @ [inh; syn]  in
         let p_descriptor  = {
           Plugin.is_polyvar = polyvar;
           Plugin.type_args  = args;
           Plugin.name       = current;
           Plugin.default    = { 
             Plugin.inh         = H.T.var inh;
             Plugin.syn         = H.T.var syn;
             Plugin.proper_args = proper_args;
             Plugin.arg_img     = (fun a -> H.T.var (img a));
           }
         } 
         in
         let derived  = map (fun name -> let Some p = Plugin.get name in name, p loc p_descriptor) deriving in
         let tpo_name = generator#generate "tpo" in
         let tpo      =
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
         let tpt = <:ctyp< < $list:combine args tpf$ > >> in
         let catype =
           let typ = H.T.app (H.T.id name :: map H.T.var args) in
           let gt  = H.T.acc [H.T.id "GT"; H.T.id "t"] in
           let ft  = H.T.arrow [H.T.var inh; typ; H.T.var syn] in
           let trt = H.T.app (H.T.class_t [class_t name] :: map H.T.var proper_args) in
           let cata_type  = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt]) ft in
           <:ctyp< $gt$ $cata_type$ >>
         in
(**)
         let metargs = (map farg args) @ [trans] in
         let args = metargs @ [acc; subj] in
         match descr with
         | (`Poly _ | `Vari _) as descr -> 
           let get_type_handler, get_local_defs, get_type_methods =
             let context = ref [] in
             let get_type_handler (ctyp, args, qname) as typ =
	       if orig_args = map (function (Variable (_, a)) -> a | _ -> "") args && qname = [current] 
	       then <:expr< $lid:"self"$ >>
	       else		  
		 let compound_name = 
                   let b = Buffer.create 64 in
                   let u =
                     let a = ref true in
                     (fun () -> if not !a then Buffer.add_string b "_"; a := false)
		   in
                   let s = Buffer.add_string b in
                   let rec filler = function
		     | Variable (_, name) -> u (); s (targ name)
                     | Instance (_, args, qname) -> 
			 iter filler args; 
                         u ();
			 iter s (map_last tname qname)
                   in
                   filler (Instance (<:ctyp< ' $"foo"$ >>, args, qname));
                   Buffer.contents b
		 in
		 let name = 
                   try fst (assoc compound_name !context) with
                     Not_found ->
		       let args =
			 let rec find_args args = function
			   | Variable (_, name) -> if mem name args then args else args @ [name]
			   | Instance (_, a, _) -> fold_left find_args args a
			 in
                         fold_left find_args [] args
                       in
		       let t = <:expr< $lid:trans$ >> in
                       let body = make_call id <:expr< $t$ # $tmethod compound_name$ >> (map (fun a -> <:expr< $lid:farg a$ >>) args) in
		       let impl = <:patt< $lid:compound_name$ >>, body in 
		       let name = <:expr< $lid:compound_name$ >> in
		       context := (compound_name, (name, (impl, args, ctyp))) :: !context;
		       name
		 in
		 name
	     in 
	     let rec funtype = function [t] -> t | h :: tl -> <:ctyp< $h$ -> $funtype tl$ >> in                              
	     get_type_handler, 
             (fun () -> map (fun (_, (_, (x, _, _))) -> x) !context),
             (fun () -> (map (fun (name, (_, (_, args, t))) -> 
                               let targs   = map (fun a -> funtype [<:ctyp< ' $inh$ >>; <:ctyp< ' $a$ >>; <:ctyp< ' $img a$ >>]) args in
                               let msig    = funtype (targs @ [<:ctyp< ' $inh$ >>; t; <:ctyp< ' $name$ >>]) in
                               <:class_str_item< method virtual $lid:tmethod name$ : $msig$ >>,
                               <:class_sig_item< method virtual $lid:tmethod name$ : $msig$ >>
                            ) 
                            !context
			) 
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

                  | `Type (Instance (_, b::args, qname)) -> 
                     let Variable (_, b) = b in
                     let qname, name = 
                       let n::t = rev qname in
                       rev ((trait_t n trait) :: t), n
                     in
                     let descr = {
                       Plugin.is_polyvar = true;
                       Plugin.type_args  = map (fun (Variable (_, a)) -> a) args; (* TODO *)
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
                      | Arbitrary t -> t
                      | Variable (t, name) -> make_a <:ctyp< ' $inh$ >> t <:ctyp< ' $img name$ >>
                      | Instance (t, _, _) -> make_a <:ctyp< ' $inh$ >> t <:ctyp< ' $syn$ >>
                      in
                      let typs = [<:ctyp< ' $inh$ >>; 
                                  let t = ctyp_of_instance loc (map (fun a -> <:ctyp< ' $a$ >>) orig_args ) (ctyp_of_qname loc [name]) in
                                   make_a <:ctyp< ' $inh$ >> t <:ctyp< ' $syn$ >>
                                 ] @ 
                                 (map make_typ cargs) 
                      in
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
                         (<:expr< $lid:acc$ >> :: 
                           (garg <:expr< $lid:"self"$ >> <:expr< $lid:subj$ >>) :: 
                           (map (fun (typ, x) -> 
                                   match typ with
                                   | Arbitrary _        -> <:expr< $lid:x$ >>
                                   | Variable (_, name) -> garg <:expr< $lid:farg name$ >> <:expr< $lid:x$ >>
                                   | Instance (typ, args, t) -> 
				       let name = get_type_handler (typ, args, t) in 
                                       garg name <:expr< $lid:x$ >>
                                ) 
                                (combine cargs args)
                           )
                         )
                    in
                    (patt, VaVal None, expr),
                    [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
                    [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>]

                | `Type (Instance (_, args, qname)) as case -> 
                    let args = map (function Variable (_, a) -> a) args in (* TODO *)
                    iter (add_derived_member case) derived;
                    let targs = flatten (map (fun a -> [a; img a]) args) @ [inh; syn] in
                    let tqname = map_last class_t qname in
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
                      let cata    = <:expr< $lid:"gcata"$ >> in
                      let func    = <:expr< $typename$ . $generic$ >> in
                      let func    = <:expr< $func$ . $cata$ >> in
                      let ext     = 
                        make_fun id [<:patt< _ >>] <:expr< $lid:"self"$ >> 
                      in
                      make_call id func 
                        ((map (fun a -> <:expr< $lid:farg a$>>) args) @ 
                         [<:expr< $lid:trans$ >>; <:expr< $lid:acc$ >>; <:expr< $lid:subj$ >>]
                        )
                    in
                    let patt =
                      let pvt  = <:patt< # $list:qname$ >> in
                      let subj = <:patt< $lid:subj$ >> in
                      <:patt< ( $pvt$ as $subj$ ) >>
                    in
                    (patt, VaVal None, expr),
                    [<:class_str_item< inherit $ce$ >>],
                    [<:class_sig_item< inherit $ct$ >>]

                | `Type (Variable (_, name)) -> oops loc (sprintf "type variable '%s is not allowed here" name)
               ) 
               (match descr with `Vari cons | `Poly cons -> cons)
           in
           let subj = <:expr< $lid:subj$ >> in 
           let local_defs_and_then expr =
             let local_defs =
                get_local_defs () @
                [<:patt< $lid:"self"$ >>  , H.E.app (H.E.id (cata current) :: map H.E.id metargs);
                 <:patt< $lid:tpo_name$ >>, tpo
                ]                                                   
             in
             match local_defs with
             | [] -> expr
             | _  -> <:expr< let rec $list:local_defs$ in $expr$ >>
           in
           let cases, methods, methods_sig    = split3 match_cases in
	   let type_methods, type_methods_sig = split (get_type_methods ()) in
           let methods      = (flatten methods) @ type_methods         in
           let methods_sig  = (flatten methods_sig) @ type_methods_sig in
           let class_expr   = <:class_expr< object $list:methods$     end >> in
           let class_type   = <:class_type< object $list:methods_sig$ end >> in
           let class_info c = { 
              ciLoc = loc;
              ciVir = Ploc.VaVal true;
              ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) proper_args));
              ciNam = Ploc.VaVal (class_t name);
              ciExp = c;
             } 
           in
           let class_def  = <:str_item< class $list:[class_info class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info class_type]$ >> in 
           let cata_name = <:patt< $lid:cata name$ >> in
           let p  = snd (fold_left (fun (i, acc) _ -> i+1, (sprintf "p%d" i)::acc) (0, []) (["t"; "acc"; "s"] @ orig_args)) in
           let pe = [generic_cata, <:expr< $lid:cata name$ >>] in 
(*
           let transformer_type =
             let orig_typ, closed_typ =
  	       let make t args =
	         fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >> ) t args
	       in
	       make <:ctyp< $lid:name$ >> orig_args, make <:ctyp< $lid:closed name$ >> orig_args
             in
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
             let extra_arg     = generator#generate "t" in             
             let extt          = <:ctyp< $ft orig_typ$ -> $ft orig_typ$ >> in
             let ft, ft_ext    = ft closed_typ, ft orig_typ in
             let cata_type     = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [<:ctyp< ' $extra_arg$ >>]) ft in
             let cata_ext_type = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [<:ctyp< ' $extra_arg$ >>; extt]) ft_ext in
             let fields        = [loc, transformer_name current, false, cata_type; loc, transformer_ext_name current, false, cata_ext_type] in
             let tdef          = <:ctyp< { $list:fields$ } >> in
             let targs         = map (fun a -> VaVal (Some a), None) (proper_args @ [extra_arg]) in
             let trt           =            
               fold_left 
                 (fun t ti -> <:ctyp< $t$ $ti$ >>) 
                 <:ctyp< # $list:[class_t name]$ >>
                 (map (fun a -> <:ctyp< ' $a$ >>) proper_args)
             in
             let tcons     = [<:ctyp< ' $extra_arg$ >>, trt] in
             let tdecl     = {tdNam = VaVal (loc, VaVal (transformer_name current)); tdPrm = VaVal targs; tdPrv = VaVal false; tdDef = tdef; tdCon = VaVal tcons} in
             tdecl
           in
*)
	   let pname = <:patt< $lid:name$ >> in
(*           transformer_type,*)
           (pname, <:expr< { $list:pe$ } >>),
(**)
           (cata_name, (make_fun (fun a -> <:patt< $lid:a$ >>) 
			  args 
			  (local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>)
		       )
	   ),
(**)
           <:sig_item< value $name$ : $catype$ >>,
           [class_def, class_decl] @ (map get_derived_classes derived)
      ) 
      d
  in
  let (*transformer_types,*) tuples (**), defs(**), decls, classes = split4 defs in
  let pnames, tnames = split tuples in
(**)  
let tuple = <:patt< ( $list:pnames$ ) >> in
  let tup = <:expr< ( $list:tnames$ ) >> in 
(**)
  let class_defs, class_decls = split (flatten classes) in
(**)
  let def = <:expr< let rec $list:defs$ in $tup$ >> in
  (**)
(**)
  let cata_def    = <:str_item< value $list:[tuple, def]$ >> in
  (**)
 (* let cata_def    = <:str_item< value rec $list:tuples$ >> in *)
  
  let type_def    = <:str_item< type $list:t$ >> in
  let type_decl   = <:sig_item< type $list:t$ >> in
(*
  let trtype_def  = <:str_item< type $list:transformer_types$ >> in
  let trtype_decl = <:sig_item< type $list:transformer_types$ >> in
*)
  <:str_item< declare $list:[type_def ]@class_defs@[(*trtype_def;*) cata_def]$ end >>,
  <:sig_item< declare $list:[type_decl]@class_decls(*@[trtype_decl]*)@decls$ end >> 
    
