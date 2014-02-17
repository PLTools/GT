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

module S = Set.Make (String)

let map_last loc f l = 
  let h, tl = hdtl loc (rev l) in
  rev (f h :: tl)
  
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
      then self#generate (prompt ^ "_")
      else (
        s := S.add prompt !s;
        prompt
      )
  end

let ctyp_of_qname loc qname =
  let module H = Plugin.Helper (struct let loc = loc end) in H.T.acc (map H.T.id qname)

let ctyp_of_instance loc args qname =
  let module H = Plugin.Helper (struct let loc = loc end) in
  H.T.app (qname :: args)

let generate t loc =
  let module H = Plugin.Helper (struct let loc = loc end) in
  let t, d = split t in
(*
  let get_cata =
    let s = fold_left (fun s ((_, n, _), _) -> S.add n s) S.empty d in
    fun name -> 
      if S.mem name s then H.E.id  (cata name)
                      else H.E.acc [H.E.id name; H.E.id "GT"; H.E.id "gcata"]
  in
*)
  let cluster_names = 
    fold_left 
      (fun acc ((_, n, d), _) -> 
         let acc = n::acc in
         match d with
         | `Poly comps->
             fold_left 
               (fun acc t ->
                  match t with
                  | `Type (_, [n]) -> n::acc
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
  let subj         = g#generate "s"                        in
  let acc          = g#generate "acc"                      in
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
         let img name      = try assoc name targs with Not_found -> oops loc "type variable image not found (should not happen)" in
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
         let derived  = map (fun name -> name, (option loc (Plugin.get name)) loc p_descriptor) deriving in
         let tpo_name = generator#generate "tpo" in
         let tpo      =
	   H.E.obj None (map (fun a -> <:class_str_item< method $lid:a$ = $H.E.id (farg a)$ >>) args )
         in
         let tpf = map (fun a -> H.T.arrow (map H.T.var [inh; a; img a])) args in
         let tpt = H.T.obj (combine args tpf) false in
         let catype =
           let typ = H.T.app (H.T.id name :: map H.T.var args) in
           let gt  = H.T.acc [H.T.id "GT"; H.T.id "t"] in
           let ft  = H.T.arrow [H.T.var inh; typ; H.T.var syn] in
           let trt = H.T.app (H.T.class_t [class_t name] :: map H.T.var proper_args) in
           H.T.app [gt; H.T.arrow (tpf @ [trt; ft])]
         in
         let metargs = (map farg args) @ [trans] in
         let args = metargs @ [acc; subj] in
         match descr with
         | (`Poly _ | `Vari _) as descr -> 
           let get_type_handler, get_local_defs, get_type_methods =
             let context = ref [] in
             let get_type_handler (ctyp, args, qname) =
	       if orig_args = args && qname = [current] 
	       then H.E.id "self"
	       else		  
		 let compound_name = 
                   let b = Buffer.create 64 in
                   let u =
                     let a = ref true in
                     (fun () -> if not !a then Buffer.add_string b "_"; a := false)
		   in
                   let s = Buffer.add_string b in
                   let filler args qname = 
		     iter (fun name -> u (); s (targ name)) args; 
                     u ();
		     iter s (map_last loc tname qname)
                   in
                   filler args qname;
                   Buffer.contents b
		 in
		 let name = 
                   try fst (assoc compound_name !context) with
                     Not_found ->
		       let args = fold_left (fun args name -> if mem name args then args else name :: args) [] args in
                       let body = H.E.app ((H.E.method_call (H.E.id trans) (tmethod compound_name)) :: 
                                            map (fun a -> H.E.id (farg a)) args
                                  ) in
		       let impl = H.P.id compound_name, body in 
		       let name = H.E.id compound_name in
		       context := (compound_name, (name, (impl, args, ctyp))) :: !context;
		       name
		 in
		 name
	     in 
	     get_type_handler, 
             (fun () -> map (fun (_, (_, (x, _, _))) -> x) !context),
             (fun () -> (map (fun (name, (_, (_, args, t))) -> 
                               let targs   = map (fun a -> H.T.arrow [H.T.var inh; H.T.var a; H.T.var (img a)]) args in
                               let msig    = H.T.arrow (targs @ [H.T.var inh; t; H.T.var name]) in
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
(*
             let addp_g      = name_generator cluster_names in
             let addp        = ref [] in
*)
             (*let register_p  = function
             | `Transform name     ->
             | `Trait (typ, trait) ->
             in*)
             (fun case (trait, (prop, p_func)) ->
                let p = option loc (Plugin.get trait) in
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
                       let body = H.E.func (map H.P.id ([constr.Plugin.acc; constr.Plugin.subj] @ args)) (p_func env constr) in
                       <:class_str_item< method $lid:name$ = $body$ >>
                     in
                     m_def

                  | `Type (args, qname) -> 
                     let qname, name = 
                       let n, t = hdtl loc (rev qname) in
                       rev ((trait_t n trait) :: t), n
                     in
                     let descr = {
                       Plugin.is_polyvar = true;
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
                    let patt    = H.P.app ((if polyvar then H.P.variant else H.P.id) cname :: map H.P.id args) in
                    let met_name = cmethod cname in
                    let met_sig  = 
                      let make_a x y z = H.T.app [H.T.acc [H.T.id "GT"; H.T.id "a"]; x; y; z; tpt] in
                      let make_typ = function
                      | Arbitrary t        -> t
                      | Variable (t, name) -> make_a (H.T.var inh) t (H.T.var (img name))
                      | Instance (t, _, _) -> make_a (H.T.var inh) t (H.T.var syn)
                      in
                      let typs = [H.T.var inh; 
                                  make_a (H.T.var inh) (H.T.app (H.T.id name :: map H.T.var orig_args)) (H.T.var syn)
                                 ] @ 
                                 (map make_typ cargs) 
                      in
		      H.T.arrow (typs @ [H.T.var syn])
                    in
                    let expr =
                      let met = H.E.method_call (H.E.id trans) met_name in
                      let garg f x =
                        H.E.app [H.E.acc [H.E.id "GT"; H.E.id "make"]; f; x; H.E.id tpo_name]
                      in
                      H.E.app (
                        [met; H.E.id acc; garg (H.E.id "self") (H.E.id subj)] @
                        (map (fun (typ, x) -> 
                                match typ with
                                | Arbitrary _        ->  H.E.id x
                                | Variable (_, name) -> garg (H.E.id (farg name)) (H.E.id x)
                                | Instance (typ, args, t) -> 
				    let name = get_type_handler (typ, args, t) in 
                                    garg name (H.E.id x)
                             ) 
                             (combine cargs args)
                        )
		     )
                    in
                    (patt, VaVal None, expr),
                    [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
                    [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>]

                | `Type (args, qname) as case -> 
                    iter (add_derived_member case) derived;
                    let targs = flatten (map (fun a -> [a; img a]) args) @ [inh; syn] in
                    let tqname = map_last loc class_t qname in
                    let targs = map H.T.var targs in
                    let ce    = <:class_expr< [ $list:targs$ ] $list:tqname$ >> in
                    let ct    =
                      let h, t = hdtl loc tqname in
                      let ct   = 
                        fold_left 
                          (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
                          <:class_type< $id:h$ >>  
                          t
                      in
                      <:class_type< $ct$ [ $list:targs$ ] >>
                    in
                    let expr =
                      let typename = H.E.acc (map H.E.id qname) in
                      H.E.app (
		        H.E.acc [typename; H.E.id "GT"; H.E.id "gcata"] ::
                        (map (fun a -> H.E.id (farg a)) args @ [H.E.id trans; H.E.id acc; H.E.id subj])
		      )
                    in
                    (H.P.alias (H.P.type_p qname) (H.P.id subj), VaVal None, expr),
                    [<:class_str_item< inherit $ce$ >>],
                    [<:class_sig_item< inherit $ct$ >>]
               ) 
               (match descr with `Vari cons | `Poly cons -> cons)
           in
           let subj = H.E.id subj in
           let local_defs_and_then expr =
             let local_defs =
                get_local_defs () @
                [H.P.id "self", H.E.app (H.E.id (cata current) :: map H.E.id metargs);
                 H.P.id tpo_name, tpo
                ]                                                   
             in
             match local_defs with
             | [] -> expr
             | _  -> H.E.letrec local_defs expr
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
	   (H.P.id name, H.E.record [generic_cata, H.E.id (cata name)]),
           (H.P.id (cata name), (H.E.func (map H.P.id args) (local_defs_and_then (H.E.match_e subj cases)))),
           <:sig_item< value $name$ : $catype$ >>,
           [class_def, class_decl] @ (map get_derived_classes derived)
      ) 
      d
  in
  let tuples, defs, decls, classes = split4 defs in
  let pnames, tnames               = split tuples in
  let class_defs, class_decls      = split (flatten classes) in
  let cata_def                     = <:str_item< value $list:[H.P.tuple pnames, H.E.letrec defs (H.E.tuple tnames)]$ >> in
  let type_def                     = <:str_item< type $list:t$ >> in
  let type_decl                    = <:sig_item< type $list:t$ >> in
  <:str_item< declare $list:type_def::class_defs@[cata_def]$ end >>,
  <:sig_item< declare $list:type_decl::class_decls@decls$ end >> 
    
