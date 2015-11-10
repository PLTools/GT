(**************************************************************************
 *  Copyright (C) 2012-2015
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
  
let split3 l = 
  List.fold_right 
    (fun (a, b, c) (x, y, z) -> a::x, b::y, c::z) l ([], [], []) 

let split4 l = 
  List.fold_right 
    (fun (a, b, c, d) (x, y, z, t) -> a::x, b::y, c::z, d::t) l ([], [], [], []) 

let split5 l = 
  List.fold_right 
    (fun (a, b, c, d, e) (x, y, z, t, h) -> a::x, b::y, c::z, d::t, e::h) l ([], [], [], [], []) 

let split6 l = 
  List.fold_right 
    (fun (a, b, c, d, e, f) (x, y, z, t, h, i) -> a::x, b::y, c::z, d::t, e::h, f::i) l ([], [], [], [], [], []) 

let ctyp_of_qname loc qname =
  let module H = Plugin.Helper (struct let loc = loc end) in H.T.acc (map H.T.id qname)

let ctyp_of_instance loc args qname =
  let module H = Plugin.Helper (struct let loc = loc end) in
  H.T.app (qname :: args)

let generate t loc =
  let module H = Plugin.Helper (struct let loc = loc end) in
  let obj_magic = <:expr< Obj.magic >> in
  let t, d = split t   in
  let d    = flatten d in
  let cluster_specs  = map (fun ((args, n, d), _) -> args, n) d in
  let get_gcata = function
    | [name] when exists (fun (_, n) -> n = name) cluster_specs -> H.E.id (cata name)
    | qname -> 
	let typename = H.E.acc (map H.E.id qname) in
        <:expr< $typename$.GT.gcata >>
  in
  let is_murec name  = try ignore (find (fun (_, n) -> name = n) cluster_specs); true with Not_found -> false in
  let reserved_names = 
    fold_left 
      (fun acc ((_, n, d), _) -> 
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
      (map snd cluster_specs)
      d 
  in
  let g     = name_generator reserved_names in
  let trans = g#generate "trans" in
  let farg  = 
    let module M = Plugin.M in
    let m = ref M.empty in
    (fun a -> 
       let p = farg a in
       try M.find p !m with
         Not_found -> 
           let n = g#generate p in
           m := M.add p n !m;
           n
    ) 
  in
  let subj          = g#generate "subj"     in
  let acc           = g#generate "inh"      in
  let generic_cata  = <:patt< GT.gcata >>   in
  let plugins_field = <:patt< GT.plugins >> in
  let defs =
    map 
      (fun ((args, name, descr), deriving) ->     
         Plugin.load_plugins deriving;
         let current       = name                                                     in
         let polyvar       = match descr with `Poly _ -> true | _ -> false            in
         let orig_args     = args                                                     in
         let generator     = name_generator args                                      in
         let targs         = map 
                               (fun arg -> arg, (generator#generate (iarg arg), generator#generate (sarg arg))) 
                               args 
         in
         let img name      = try assoc name targs 
                             with Not_found -> oops loc "type variable image not found (should not happen)" 
         in
         let iname name    = fst (img name) in
         let sname name    = snd (img name) in
         let inh           = generator#generate "inh"                                         in
         let syn           = generator#generate "syn"                                         in
         let proper_args   = flatten (map (fun (x, (y, z)) -> [x; y; z]) targs) @ [inh; syn]  in
         let p_descriptor  = {
           Plugin.is_polyvar = polyvar;
           Plugin.type_args  = args;
           Plugin.name       = current;
           Plugin.default    = { 
             Plugin.inh_t       = H.T.var inh;
             Plugin.syn_t       = H.T.var syn;
             Plugin.proper_args = proper_args;
   	     Plugin.fixed_inh   = None;
             Plugin.sname       = (fun a -> H.T.var (sname a));
             Plugin.iname       = (fun a -> H.T.var (iname a))
           }
         } 
         in
         let derived = map (fun name -> name, (option loc (Plugin.get name)) loc p_descriptor) deriving in
         let plugin_field_type, plugin_field_expr = 
           let method_decls, method_defs = split (
	     map 
	       (fun (trait, (prop, generator)) -> 
		  let fixed_inh    = prop.Plugin.fixed_inh                  in
		  let g            = name_generator [current]               in
		  let arg_names    = map g#generate args                    in
                  let arg_types    = map H.T.var args                       in
		  let current_type = H.T.app (H.T.id current :: arg_types)  in
                  let arg_tr_exprs = 
		    map 
		      (fun a -> 
		        match fixed_inh with 
			| None   -> H.E.id a
			| Some _ -> <:expr< GT.lift $H.E.id a$ >>
		      ) 
		      arg_names 
		  in
		  let arg_tr_types =
		    map 
		      (fun a ->
			match fixed_inh with
			| None   -> H.T.arrow [prop.Plugin.iname a; H.T.var a; prop.Plugin.sname a]
			| Some _ -> H.T.arrow [H.T.var a; prop.Plugin.sname a]
		      )
		      args
		  in
		  let method_type = 
		    match fixed_inh with
		    | None   -> H.T.arrow (arg_tr_types @ [prop.Plugin.inh_t; current_type; prop.Plugin.syn_t])
		    | Some _ -> H.T.arrow (arg_tr_types @ [current_type; prop.Plugin.syn_t])
		  in
		  let method_body =
		    match fixed_inh with
		    | None -> 
			H.E.func 
			  (map H.P.id arg_names)
			  (H.E.app (<:expr< GT.transform >> :: H.E.id current :: (arg_tr_exprs @ [H.E.new_e [trait_t current trait]])))
		    | Some inh ->
			H.E.func 
			  (map H.P.id arg_names)
			  (H.E.app (<:expr< GT.transform >> :: H.E.id current :: (arg_tr_exprs @ [H.E.new_e [trait_t current trait]] @ [inh])))
		  in
		  let method_body = generator#default method_body in
		  (trait, method_type), 
                  <:class_str_item< method $trait$ = $method_body$ >>
	       ) 
	       derived 
           )
	   in
           H.T.obj method_decls false, H.E.obj None method_defs	   
	 in
         let tpo_name  = generator#generate "tpo" in
	 let self_name = generator#generate "self" in
         let tpo =
	   H.E.obj None (map (fun a -> <:class_str_item< method $lid:a$ = $H.E.id (farg a)$ >>) args )
         in
         let tpf = map (fun a -> H.T.arrow (map H.T.var [iname a; a; sname a])) args in
         let tpt = H.T.obj (combine args tpf) false in
         let catype plugins_type =
           let typ = H.T.app (H.T.id name :: map H.T.var args) in
           let gt  = H.T.acc [H.T.id "GT"; H.T.id "t"] in
           let ft  = H.T.arrow [H.T.var inh; typ; H.T.var syn] in
           let trt = H.T.app (H.T.class_t [class_tt name] :: map H.T.var proper_args) in
           H.T.app [gt; H.T.arrow (tpf @ [trt; ft]); plugins_type]
         in
         let metargs = (map farg args) @ [trans] in
         let args = metargs @ [acc; subj] in
         let get_type_handler, get_local_defs, get_type_methods =
           let method_decls = ref [current, (H.E.id current, (orig_args, H.T.app (H.T.id current :: map H.T.var orig_args)))] in
           let method_defs  = ref [] in
           let get_type_handler (ctyp, args, qname) =
	     if orig_args = args && qname = [current] 
	     then H.E.id self_name
	     else		  
	       let compound_name = 
                 let b = Buffer.create 64 in
                 let u =
                   let a = ref true in
                   (fun () -> if not !a then Buffer.add_string b "_"; a := false)
		 in
                 let s = Buffer.add_string b in
                 let filler args qname = 
		   iter (fun name -> u (); s name) args;                      
		   iter (fun name -> u (); s name) qname
                 in
                 filler args qname;
                 Buffer.contents b
	       in
	       let name = 
                 try fst (assoc compound_name !method_decls) with
                   Not_found ->
		     let args = fold_left (fun args name -> if mem name args then args else name :: args) [] args in
                     let body = H.E.app ((H.E.method_call (H.E.id trans) (tmethod compound_name)) :: 
                                         map (fun a -> H.E.id (farg a)) args
					) in
		     let impl = H.P.id compound_name, body in 
		     let name = H.E.id compound_name in
		     method_decls := (compound_name, (name, (args, ctyp))) :: !method_decls;
                     method_defs  := impl :: !method_defs;
		     name
	       in
	       name
	   in 
	   get_type_handler, 
           (fun () -> !method_defs),
           (fun () -> (map 
			 (fun (name, (_, (args, t))) -> 
			   let targs   = map (fun a -> H.T.arrow [H.T.var (iname a); H.T.var a; H.T.var (sname a)]) args in
			   let msig    = H.T.arrow (targs @ [H.T.var inh; t; H.T.var syn]) in
			   <:class_str_item< method virtual $lid:tmethod name$ : $msig$ >>,
			   <:class_sig_item< method $lid:tmethod name$ : $msig$ >>
                         )
                         !method_decls
		      ) 
           )
         in
	 let add_derived_member, get_derived_classes =
           let murec = length cluster_specs > 1 in
           let module M = 
	     struct

	       type t = {
		 gen         : < generate : string -> string; copy : 'a > as 'a;
                 proto_items : class_str_item list;
		 items       : class_str_item list;
                 defaults    : class_str_item list;
                 self_name   : string;
                 in_cluster  : bool;
		 this        : string;
		 self        : string;
		 env         : string;
                 env_sig     : class_sig_item list;
               }

	       module M = Plugin.M

	       let m = ref M.empty
	       let get trait = 
		 try M.find trait !m 
		 with Not_found -> 
		   let g    = name_generator reserved_names in
		   let this = g#generate "this" in
		   let env  = g#generate "env"  in
		   let self = g#generate "self" in
		   let cn   = g#generate ("c_" ^ name) in
                   let vals, inits, methods, env_methods = split4 (
		     map 
		       (fun (args, t) ->
			 let ct      = if name = t then cn else g#generate ("c_" ^ t) in
			 let proto_t = trait_proto_t t trait in
			 let mt      = tmethod t             in
			 let args          = map g#generate args in
			 let targs         = map (fun a -> a, (g#generate (iarg a), g#generate (sarg a))) args in
                         let p_descriptor  = {
                           Plugin.is_polyvar = false;
                           Plugin.type_args  = args;
                           Plugin.name       = t;
                           Plugin.default    = { 
                             Plugin.inh_t       = H.T.var inh;
                             Plugin.syn_t       = H.T.var syn;
                             Plugin.proper_args = args;
                             Plugin.fixed_inh   = None;
                             Plugin.sname       = (fun a -> H.T.var (snd (assoc a targs)));
                             Plugin.iname       = (fun a -> H.T.var (fst (assoc a targs)))
                           }
                         } 
			 in
                         let prop, _ = (option loc (Plugin.get trait)) loc p_descriptor in
                         let typ     = H.T.app (H.T.id t :: map H.T.var args) in
			 let inh_t   = prop.Plugin.inh_t in
			 let targs   = map (fun a -> H.T.arrow [prop.Plugin.iname a; H.T.var a; prop.Plugin.sname a]) p_descriptor.Plugin.type_args in
			 let mtype   = H.T.arrow (targs @ [inh_t; typ; prop.Plugin.syn_t]) in
			 <:class_str_item< value mutable $lid:ct$ = $H.E.app [obj_magic; H.E.unit]$ >>,
			 (H.E.assign (H.E.id ct) (H.E.app [H.E.new_e [proto_t]; H.E.id self])),
			 <:class_str_item< method $mt$ : $mtype$ = $H.E.method_call (H.E.id ct) mt$ >>,
			 <:class_sig_item< method $tmethod t$ : $mtype$ >>			   
		       )
		       (filter (fun (_, n) -> n <> name) cluster_specs)
		    )
		   in
		   let items = 
		     let prop, _ = (option loc (Plugin.get trait)) loc p_descriptor in
		     let this    = H.E.coerce (H.E.id this) (H.T.app (H.T.id (trait_t name trait)::map H.T.var prop.Plugin.proper_args)) in
		     vals @ [<:class_str_item< initializer $H.E.seq (H.E.app [H.E.lid ":="; H.E.id self; this]::inits)$ >>] @ methods 
		   in
		   {gen         = g; 
		    this        = this;
		    self        = self;
		    env         = env;
                    env_sig     = env_methods;
		    proto_items = []; 
		    items       = items;
                    defaults    = [];
                    in_cluster  = murec;
                    self_name   = cn;
		  }

               let put trait t = m := M.add trait t !m

	     end
	   in
           (fun case (trait, (prop, generator)) ->
             let p       = option loc (Plugin.get trait) in
             let context = M.get trait                   in
	     let g       = context.M.gen#copy            in
	     let branch met_name met_args gen =
               let rec env = {
                 Plugin.inh      = g#generate "inh";
                 Plugin.subj     = g#generate "subj";
                 Plugin.this     = context.M.this;
                 Plugin.new_name = (fun s -> g#generate s); 
                 Plugin.trait    = 
		   (fun s t -> 			     
		      if s = trait
		      then
			let rec inner = function
			| Variable (_, a) -> H.E.gt_tp (H.E.id env.Plugin.subj) a
			| Instance (_, args, qname) -> 
			    let args = map inner args in
			    (match qname with
                             | [t] when is_murec t && t <> current -> H.E.app ((H.E.method_call (H.E.app [H.E.lid "!"; H.E.id context.M.env]) (tmethod t)) :: args)
			     | _  -> 
                 		(match qname with 
				| [t] when t = current -> H.E.app ([H.E.acc (map H.E.id ["GT"; "transform"]); H.E.acc (map H.E.id qname)] @ args @ [H.E.id "this"]) 
				| _ -> let s = H.E.acc (map H.E.id (qname@["GT";"plugins"])) in
                                       let m = H.E.method_call s trait in                                          
                                       match prop.Plugin.fixed_inh with
				       | None   -> H.E.app (m::args)
                                       | Some e -> 
                                           let args = map (fun a -> <:expr< $a$ $e$ >>) args in 
                                           let b = H.E.app (m::args) in 
                                           <:expr< GT.lift$b$ >>
                                )      
			    )
			| Self _ -> H.E.gt_f (H.E.id env.Plugin.subj)
			| _ -> invalid_arg "Unsupported type"			    
			in (try Some (inner t) with Invalid_argument "Unsupported type" -> None)
		      else None
		   )
               } 
               in
               let m_def = 
                 let body = H.E.func (map H.P.id ([env.Plugin.inh; env.Plugin.subj] @ met_args)) (gen env) in
                 <:class_str_item< method $lid:met_name$ = $body$ >>
               in
               {context with M.proto_items = m_def :: context.M.proto_items}
	     in
             let context =
               match case with
	       | `Struct fields -> 
		   let fields = map (fun (n, m, t) -> g#generate n, (n,  m, t)) fields in
                   branch vmethod (map fst fields) (fun env -> generator#record env fields)

	       | `Tuple elems ->
		   let elems = mapi (fun i t -> g#generate (sprintf "p%d" i), t) elems in
		   branch vmethod (map fst elems) (fun env -> generator#tuple env elems)

               | `Con (cname, cargs) -> 
		   let args = mapi (fun i a -> g#generate (sprintf "p%d" i), a) cargs in
		   branch (cmethod cname) (map fst args) (fun env -> generator#constructor env cname args)

               | `Type (Instance (_, args, qname)) -> 
		   let args = map (function Variable (_, a) -> a | _ -> oops loc "unsupported case (non-variable in instance)") args in (* TODO *)
                   let qname, qname_proto, env_tt, name = 
                     let n, t = hdtl loc (rev qname) in
                     rev ((trait_t n trait) :: t), 
		     rev ((trait_proto_t n trait) :: t), 
		     rev ((env_tt n trait) :: t), 
		     n
                   in
                   let descr = {
                     Plugin.is_polyvar = polyvar;
                     Plugin.type_args  = args;
                     Plugin.name       = name;
                     Plugin.default    = prop;
                   }
                   in
                   let prop               = fst (p loc descr) in
                   let i_def      , _     = Plugin.generate_inherit false loc qname_proto (Some (H.E.id context.M.self, H.T.id "unit")) descr prop in  
                   let i_impl     , _     = Plugin.generate_inherit false loc qname None descr prop in  
                   let i_def_proto, _     = Plugin.generate_inherit false loc qname_proto (Some (H.E.id context.M.env, H.T.id "unit")) descr prop in
		   let _          , i_env = Plugin.generate_inherit false loc env_tt      None descr prop in
                   {context with M.defaults = i_impl :: context.M.defaults;
                    M.items = i_def :: context.M.items;  
		    M.proto_items = i_def_proto :: context.M.proto_items;
                    M.env_sig     = i_env :: context.M.env_sig
		   }
	       | _ -> oops loc "unsupported case (infernal error)"
             in
	     M.put trait context
           ),
           (fun (trait, p) -> 
 	      let context       = M.get trait in 
              let c_def, c_decl = List.split (snd p)#custom in
              let i_def, _      = Plugin.generate_inherit true  loc [class_t  current] None p_descriptor (fst p) in
              let _    , i_decl = Plugin.generate_inherit true  loc [class_tt current] None p_descriptor (fst p) in
	      let p_def, _      = Plugin.generate_inherit false loc [trait_proto_t current trait] (Some (H.E.id context.M.self, H.T.id "unit")) p_descriptor (fst p) in
              let cproto        = <:class_expr< object ($H.P.id context.M.this$) $list:i_def::context.M.proto_items@c_def$ end >> in
              let ce            = 
	        let ce = <:class_expr< object ($H.P.id context.M.this$) $list:i_def::p_def::context.M.defaults@context.M.items@c_def$ end >> in
	        <:class_expr< let $flag:false$ $list:[H.P.id context.M.self, H.E.app [obj_magic; H.E.app [H.E.id "ref"; H.E.unit]]]$ in $ce$ >>
	      in
              let env_t         = <:class_type< object $list:context.M.env_sig$ end >> in
	      let class_targs   = map H.T.var (fst p).proper_args in
              let cproto_t      = <:class_type< [ $H.T.app [H.T.id "ref"; H.T.app (H.T.id (env_tt current trait) :: class_targs)]$ ] -> object $list:i_decl::c_decl$ end >> in
	      let ct            = 
                let ct = 
		  match class_targs with 
		  | [] -> <:class_type< $id:env_tt current trait$ >>  
		  | _  -> <:class_type< $id:env_tt current trait$ [ $list:class_targs$ ] >> 
		in
	        let env_inh = <:class_sig_item< inherit $ct$ >> in
	        <:class_type< object $list:i_decl::env_inh::c_decl$ end >> 
	      in
              Plugin.generate_classes loc trait p_descriptor p (context.M.this, context.M.env, env_t, cproto, ce, cproto_t, ct)
	   )
         in
         let match_cases =
	   let case_branch patt met_name names types =
	     let met_sig  = 
               let make_a x y z = H.T.app [<:ctyp< GT.a >>; x; y; z; tpt] in
               let rec make_typ = function
               | Arbitrary t | Instance (t, _, _) -> t
               | Variable (t, name) -> make_a (H.T.var (iname name)) t (H.T.var (sname name))
               | Self     (t, _, _) -> make_a (H.T.var inh) t (H.T.var syn)
	       | Tuple    (t, typs) -> H.T.tuple (map make_typ typs)
               in
               let typs = [H.T.var inh; 
                           make_a (H.T.var inh) (H.T.app (H.T.id name :: map H.T.var orig_args)) (H.T.var syn)
                          ] @ 
                          (map make_typ types) 
               in
	       H.T.arrow (typs @ [H.T.var syn])
             in
             let expr =
               let met = H.E.method_call (H.E.id trans) met_name in
               let garg f x =
                 H.E.app [<:expr< GT.make >>; f; x; H.E.id tpo_name]
               in
               H.E.app (
                 [met; H.E.id acc; garg (H.E.id self_name) (H.E.id subj)] @
                 (map (fun (typ, x) -> 
		         let rec augmented = function
			 | Arbitrary _ | Instance _ -> false
			 | Self      _ | Variable _ -> true
			 | Tuple (_, typs) -> exists augmented typs
			 in             
			 let rec augment id = function
                         | Arbitrary _ | Instance _ ->  H.E.id id
                         | Variable (_, name)       -> garg (H.E.id (farg name)) (H.E.id id)
                         | Self     (typ, args, t)  -> 
			     let name = get_type_handler (typ, args, t) in 
			     garg name (H.E.id id)
			 | Tuple    (_, typs) as typ ->
			     if augmented typ 
			     then
			       let generator  = generator#copy in
                               let components = mapi (fun i _ -> generator#generate (sprintf "e%d" i)) typs in
			       H.E.let_nrec 
                                 [H.P.tuple (map H.P.id components), H.E.id id] 
                                 (H.E.tuple (map (fun (name, typ) -> augment name typ) (combine components typs)))
			     else H.E.id id
			 in
			 augment x typ
                      ) 
                      (combine types names)
                 )
	       )
             in
             (patt, VaVal None, expr),
             [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
             [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>],
             [<:class_sig_item< method $lid:met_name$ : $met_sig$ >>],
	     []	       
	   in
           map 
             (function 
              | `Tuple elems as case -> 
                  iter (add_derived_member case) derived;
                  let args = mapi (fun i a -> sprintf "p%d" i) elems in
                  let patt = H.P.tuple (map H.P.id args) in
                  case_branch patt vmethod args elems

	      | `Struct fields as case ->
		  iter (add_derived_member case) derived;
		  let names, _, types = split3 fields in		  
                  let args = map (fun a -> generator#generate a) names in
                  let patt = H.P.record (map (fun (n, a) -> H.P.id n, H.P.id a) (combine names args)) in
		  case_branch patt vmethod args types

              | `Con (cname, cargs) as case -> 
                  iter (add_derived_member case) derived;
                  let args = mapi (fun i a -> sprintf "p%d" i) cargs in
                  let patt = H.P.app ((if polyvar then H.P.variant else H.P.id) cname :: map H.P.id args) in
                  case_branch patt (cmethod cname) args cargs

              | `Type (Instance (_, args, qname)) as case -> 
		  let args = map (function Variable (_, a) -> a | _ -> oops loc "unsupported case (non-variable in instance)") args in (* TODO *)
                  iter (add_derived_member case) derived;
                  let targs = flatten (map (fun a -> [a; iname a; sname a]) args) @ [inh; syn] in
                  let targs = map H.T.var targs in
                  let ce    = <:class_expr< [ $list:targs$ ] $list:map_last loc class_t qname$ >> in
                  let ct f  =
                    let h, t = hdtl loc (map_last loc f qname) in
                    let ct   = 
                      fold_left 
                        (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
                        <:class_type< $id:h$ >>  
                      t
                    in
                    <:class_type< $ct$ [ $list:targs$ ] >>
                  in
                  let expr =
                    H.E.app (
                      (get_gcata qname) ::
                      (map (fun a -> H.E.id (farg a)) args @ [H.E.id trans; H.E.id acc; H.E.id subj])
		   )
                  in
                  (H.P.alias (H.P.type_p qname) (H.P.id subj), VaVal None, expr),
                  [<:class_str_item< inherit $ce$ >>],
                  [<:class_sig_item< inherit $ct class_t$  >>],
                  [<:class_sig_item< inherit $ct class_tt$ >>],
		  [args, qname]

	      | _ -> oops loc "unsupported case (internal error)"
             ) 
             (match descr with `Vari cons | `Poly cons -> cons | (`Tuple _ | `Struct _) as x -> [x])
         in
         let subj = H.E.id subj in
         let local_defs_and_then expr =
           let local_defs =
             get_local_defs () @
             [H.P.id self_name, H.E.app (H.E.id (cata current) :: map H.E.id metargs);
              H.P.id tpo_name, tpo
             ]                                                   
           in
           match local_defs with
           | [] -> expr
           | _  -> H.E.letrec local_defs expr
         in
         let cases, methods, methods_sig, methods_sig_t, base_types = split5 match_cases in
	 let type_methods, type_methods_sig                         = split (get_type_methods ()) in
	 let base_types       = flatten base_types                   in
	 let is_abbrev        = not polyvar && length base_types = 1 in
         let methods          = flatten methods                      in
         let methods_sig      = flatten methods_sig                  in
	 let methods_sig_t    = flatten methods_sig_t                in
         (* proto_class_type -> meta_class_type *)
         let proto_class_type = <:class_type< object $list:methods_sig_t@type_methods_sig$ end >> in
         let class_expr = 
	   let this = generator#generate "this" in
           let body = 
	     let args = map farg orig_args in 
	     H.E.func (map H.P.id args) (H.E.app ((H.E.acc (map H.E.id ["GT"; "transform"])) :: map H.E.id (name::args@[this])))
	   in
           let met = <:class_str_item< method $lid:tmethod name$ = $body$ >> in
           <:class_expr< object ($H.P.id this$) $list:methods@[met]$ end >> 
         in
         let class_type = <:class_type< object $list:methods_sig@type_methods_sig$ end >> in
         let class_info v name c = { 
           ciLoc = loc;
           ciVir = Ploc.VaVal v;
           ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) proper_args));
           ciNam = Ploc.VaVal name;
           ciExp = c;
         } 
         in           
         (*
           let meta_class_def  = ... meta_class_type
           let meta_class_decl = ... meta_class_type

         *)
         let type_class_def  = <:str_item< class type $list:[class_info true (class_tt name) proto_class_type]$ >> in
         let type_class_decl = <:sig_item< class type $list:[class_info true (class_tt name) proto_class_type]$ >> in 
         let class_def  = <:str_item< class $list:[class_info true (class_t name) class_expr]$ >> in
         let class_decl = <:sig_item< class $list:[class_info true (class_t name) class_type]$ >> in
	 let body = 
	   if is_abbrev 
	   then let (_, _, expr), _ = hdtl loc cases in expr
	   else local_defs_and_then (H.E.match_e subj cases)
	 in	
	 (H.P.constr (H.P.id name) (catype (H.T.id "unit")), H.E.record [generic_cata, H.E.id (cata name); plugins_field, H.E.unit]),
         (H.P.id (cata name), (H.E.func (map H.P.id args) body)),
         <:sig_item< value $name$ : $catype plugin_field_type$ >>,
         (type_class_def, type_class_decl),
         (let env, protos, defs, edecls, pdecls, decls = split6 (map get_derived_classes derived) in
          class_def, (flatten env)@protos, defs, class_decl::(flatten edecls)@pdecls@decls 
	 ),
         <:str_item< value $list:[(H.P.constr (H.P.id name) (catype plugin_field_type),
                                  (H.E.record [generic_cata, H.E.acc [H.E.id name; <:expr< GT.gcata >>]; plugins_field, plugin_field_expr])
                                 )]$ >>
      ) 
      d
  in
  let open_type td =   
    match td.tdDef with
    | <:ctyp< [ = $list:lcons$ ] >> ->
	let get_val x = get_val loc x in
        let name      = type_open_t (get_val (snd (get_val td.tdNam))) in
	let args      = map (function (VaVal (Some name), _) -> name | _ -> oops loc "unsupported case (internal error)") (get_val td.tdPrm) in
	let gen       = name_generator args in
        let self      = gen#generate "self" in
	[{td with tdNam = VaVal (loc, VaVal name);
	          tdPrm = VaVal (map (fun name -> VaVal (Some name), None) (self::args));
	          tdDef = H.T.var self;
                  tdCon = VaVal [H.T.var self, <:ctyp< [> $list:lcons$ ]>>]
	 }
        ]
    | _ -> []
  in
  let tuples, defs, decls, classes, derived_classes, with_plugins = split6 defs         in
  let pnames,  tnames                                             = split  tuples       in
  let class_defs, class_decls                                     = split  classes      in
  let derived_class_defs, derived_class_decls                     = 
    let class_defs, protos, defs, class_decls = split4 derived_classes in
    class_defs@(flatten protos)@(flatten defs), flatten class_decls
  in
  let cata_def = 
    if length pnames < 2 
    then <:str_item< value $list:[hd pnames, H.E.letrec defs (hd tnames)]$ >> 
    else <:str_item< value $list:[H.P.tuple pnames, H.E.letrec defs (H.E.tuple tnames)]$ >> 
  in
  let open_t         = flatten (map open_type t) in
  let type_def       = <:str_item< type $list:t@open_t$ >> in
  let type_decl      = <:sig_item< type $list:t@open_t$ >> in
  <:str_item< declare $list:type_def::class_defs@[]@[cata_def]@derived_class_defs@with_plugins$ end >>,
  <:sig_item< declare $list:type_decl::class_decls@decls@derived_class_decls$ end >> 
    
