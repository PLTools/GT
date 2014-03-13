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

let map_last loc f l = 
  let h, tl = hdtl loc (rev l) in
  rev (f h :: tl)
  
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
  let t, d = split t in
  let cluster_specs  = map (fun ((args, n, d), _) -> args, n) d in
  let in_cluster     = match cluster_specs with [_] -> false | _ -> true in
  let is_murec name  = try ignore (find (fun (_, n) -> name = n) cluster_specs); true with Not_found -> false in
  let reserved_names = 
    fold_left 
      (fun acc ((_, n, d), _) -> 
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
      (map snd cluster_specs)
      d 
  in
  let g     = name_generator reserved_names in
  let trans = g#generate "trans" in
  let farg  = 
    let module M = Map.Make (String) in
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
  let subj         = g#generate "subj"                     in
  let acc          = g#generate "inh"                      in
  let generic_cata = H.P.acc [H.P.id "GT"; H.P.id "gcata"] in
  let defs =
    map 
      (fun ((args, name, descr), deriving) ->     
         Plugin.load_plugins deriving;
         let current       = name                                                     in
         let polyvar       = match descr with `Poly _ -> true | _ -> false            in
         let orig_args     = args                                                     in
         let generator     = name_generator args                                      in
         let targs         = map (fun arg -> arg, generator#generate (targ arg)) args in
         let img name      = try assoc name targs with Not_found -> 
                               oops loc "type variable image not found (should not happen)" in
         let inh           = generator#generate "inh"                                 in
         let syn           = generator#generate "syn"                                 in
         let proper_args   = flatten (map (fun (x, y) -> [x; y]) targs) @ [inh; syn]  in
         let p_descriptor  = {
           Plugin.is_polyvar = polyvar;
           Plugin.type_args  = args;
           Plugin.name       = current;
           Plugin.default    = { 
             Plugin.inh_t       = H.T.var inh;
             Plugin.syn_t       = H.T.var syn;
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
           let trt = H.T.app (H.T.class_t [class_tt name] :: map H.T.var proper_args) in
           H.T.app [gt; H.T.arrow (tpf @ [trt; ft])]
         in
         let metargs = (map farg args) @ [trans] in
         let args = metargs @ [acc; subj] in
         match descr with
         | (`Poly _ | `Vari _) as descr -> 
           let get_type_handler, get_local_defs, get_type_methods =
             let method_decls = ref [current, (H.E.id current, (orig_args, H.T.app (H.T.id current :: map H.T.var orig_args)))] in
             let method_defs  = ref [] in
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
             (fun () -> (map (fun (name, (_, (args, t))) -> 
                               let targs   = map (fun a -> H.T.arrow [H.T.var inh; H.T.var a; H.T.var (img a)]) args in
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
		   gen         : < generate : string -> string >;
                   proto_items : class_str_item list;
		   items       : class_str_item list;
                   self_name   : string;
                   in_cluster  : bool;
		   this        : string;
		   env         : string;
                   env_sig     : class_sig_item list;
                 }

		 module M = Map.Make (String) 

		 let m = ref M.empty
		 let get trait = 
		   try M.find trait !m 
		   with Not_found -> 
		     let g    = name_generator reserved_names in
		     let this = g#generate "this" in
		     let env  = g#generate "env"  in
		     let cn   = g#generate ("c_" ^ name) in
                     let vals, inits, methods, env_methods = split4 (
		       map 
			 (fun (args, t) ->
			   let ct      = if name = t then cn else g#generate ("c_" ^ t) in
			   let proto_t = trait_proto_t t trait in
			   let mt      = tmethod t             in
			   <:class_str_item< value mutable $lid:ct$ = $H.E.app [H.E.acc [H.E.id "Obj"; H.E.id "magic"]; H.E.unit]$ >>,
			   (H.E.assign (H.E.id ct) (H.E.app [H.E.new_e [proto_t]; H.E.id this])),
			   (if t <> name then [<:class_str_item< method $mt$ = $H.E.method_call (H.E.id ct) mt$ >>] else []),
			   (if t <> name 
			    then 
			      let args          = map g#generate args in
			      let targs         = map (fun a -> a, g#generate (targ a)) args in
                              let p_descriptor  = {
                                Plugin.is_polyvar = false;
                                Plugin.type_args  = args;
                                Plugin.name       = t;
                                Plugin.default    = { 
                                  Plugin.inh_t       = H.T.var inh;
                                  Plugin.syn_t       = H.T.var syn;
                                  Plugin.proper_args = args;
                                  Plugin.arg_img     = (fun a -> H.T.var (assoc a targs));
                                }
                              } 
			      in
                              let prop, _ = (option loc (Plugin.get trait)) loc p_descriptor in
                              let typ     = H.T.app (H.T.id t :: map H.T.var args) in
			      let targs   = map (fun a -> H.T.arrow [prop.Plugin.inh_t; H.T.var a; prop.Plugin.arg_img a]) prop.Plugin.proper_args in
			      [<:class_sig_item< method $tmethod t$ : $H.T.arrow (targs @ [prop.Plugin.inh_t; typ; prop.Plugin.syn_t])$ >>]
			    else []
			   )
			 ) 
			 cluster_specs
		      )
		     in
		     let items = vals @ [<:class_str_item< initializer $H.E.seq inits$ >>] @ (flatten methods) in
		     {gen         = g; 
		      this        = this;
		      env         = env;
                      env_sig     = flatten env_methods;
		      proto_items = []; 
		      items       = items;
                      in_cluster  = murec;
                      self_name   = cn;
		     }

                 let put trait t = 
                   m := M.add trait t !m

	       end
	     in
             (fun case (trait, (prop, p_func)) ->
                let p       = option loc (Plugin.get trait) in
                let context = M.get trait in
		let g       = context.M.gen in
                let context =
                  match case with
                  | `Con (cname, cargs) ->
                     let args = fst (fold_right (fun _ (acc, i) -> (g#generate (sprintf "p%d" i))::acc, i+1) cargs ([], 0)) in
                     let constr = {
                       Plugin.constr = cname;
                       Plugin.inh    = g#generate "inh";
                       Plugin.subj   = g#generate "subj";
                       Plugin.args   = combine args cargs;
                     }
                     in
                     let env = {
                       Plugin.new_name = (fun s -> g#generate s); 
                       Plugin.trait    = 
		         (fun s t -> 
			   if s = trait 
			   then 
			     let apply x = function None -> x | Some y -> H.E.app [x; y] in
			     let rec qname = function
			       | <:ctyp< $lid:x$ >> | <:ctyp< $uid:x$ >> -> [x]
			       | <:ctyp< $t1$ . $t2$ >> -> qname t1 @ qname t2
			       | _ -> invalid_arg "Unsupported type"
			     in
			     let rec inner = function
			       | <:ctyp< ' $a$ >>     -> H.E.gt_tp (H.E.id constr.Plugin.subj) a, None
			       | <:ctyp< $t1$ $t2$ >> -> 
				   let t1, tt1 = inner t1 in
				   let t2, tt2 = inner t2 in
				   H.E.app [t1; apply t2 tt2], tt1
			       | t -> 
				   let qname = qname t in
				   (match qname with
				   | [t] when is_murec t && t <> current -> H.E.method_call (H.E.id context.M.env) (tmethod t), None
				   | _  -> 
				       let tobj = 
					 match qname with 
					 | [t] when t = current -> H.E.id "this"
					 | _ -> H.E.new_e (map_last loc (fun name -> trait_t name trait) qname) 
				       in
				       H.E.app [H.E.acc (map H.E.id ["GT"; "transform"]); H.E.acc (map H.E.id qname)], Some tobj
				   )
			     in
                             (try let t, tt = inner t in Some (apply t tt) with Invalid_argument "Unsupported type" -> None)
			   else None
			 ); 
                     } 
                     in
                     let m_def = 
                       let name = cmethod cname in
                       let body = H.E.func (map H.P.id ([constr.Plugin.inh; constr.Plugin.subj] @ args)) (p_func env constr) in
                       <:class_str_item< method $lid:name$ = $body$ >>
                     in
		     let bridge_def = 
		       let name = cmethod cname in
		       let body = H.E.method_call (H.E.id context.M.self_name) name in
		       <:class_str_item< method $lid:name$ = $body$ >>
		     in
                     {context with M.proto_items = m_def :: context.M.proto_items; M.items = context.M.items @ [bridge_def]}

                  | `Type (args, qname) -> 
                     let qname, qname_proto, env_tt, name = 
                       let n, t = hdtl loc (rev qname) in
                       rev ((trait_t n trait) :: t), 
		       rev ((trait_proto_t n trait) :: t), 
		       rev ((env_tt n trait) :: t), 
		       n
                     in
                     let descr = {
                       Plugin.is_polyvar = true;
                       Plugin.type_args  = args;
                       Plugin.name       = name;
                       Plugin.default    = prop;
                     }
                     in
		     let prop               = fst (p loc descr) in
                     let i_def      , _     = Plugin.generate_inherit false loc qname       None descr prop in
                     let i_def_proto, _     = Plugin.generate_inherit false loc qname_proto (Some (H.E.id context.M.env, H.T.id "unit")) descr prop in
		     let _          , i_env = Plugin.generate_inherit false loc env_tt      None descr {prop with Plugin.proper_args = []} in
                     {context with M.items = i_def :: context.M.items; 
		                   M.proto_items = i_def_proto :: context.M.proto_items;
                                   M.env_sig     = i_env :: context.M.env_sig
		     }
                in
		M.put trait context
             ),
             (fun (trait, p) -> 
	       let context       = M.get trait in 
               let i_def, _      = Plugin.generate_inherit true loc [class_t  current] None p_descriptor (fst p) in
               let _    , i_decl = Plugin.generate_inherit true loc [class_tt current] None p_descriptor (fst p) in
               let cproto        = <:class_expr< object ($H.P.id context.M.this$) $list:i_def::context.M.proto_items$ end >> in
               let ce            = <:class_expr< object ($H.P.id context.M.this$) $list:i_def::context.M.items$ end >> in
               let env_t         = <:class_type< object $list:context.M.env_sig$ end >> in
               let cproto_t      = <:class_type< [ $H.T.id (env_tt current trait)$ ] -> object $list:[i_decl]$ end >> in
	       let ct            = 
                 let ct = <:class_type< $id:env_tt current trait$ >> in
		 let env_inh = <:class_sig_item< inherit $ct$ >> in
		 <:class_type< object $list:[i_decl; env_inh]$ end >> 
	       in
               Plugin.generate_classes loc trait p_descriptor p (context.M.this, context.M.env, env_t, cproto, ce, cproto_t, ct)
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
                    [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>],
                    [<:class_sig_item< method $lid:met_name$ : $met_sig$ >>]

                | `Type (args, qname) as case -> 
                    iter (add_derived_member case) derived;
                    let targs = flatten (map (fun a -> [a; img a]) args) @ [inh; syn] in
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
                      let typename = H.E.acc (map H.E.id qname) in
                      H.E.app (
		        H.E.acc [typename; H.E.id "GT"; H.E.id "gcata"] ::
                        (map (fun a -> H.E.id (farg a)) args @ [H.E.id trans; H.E.id acc; H.E.id subj])
		      )
                    in
                    (H.P.alias (H.P.type_p qname) (H.P.id subj), VaVal None, expr),
                    [<:class_str_item< inherit $ce$ >>],
                    [<:class_sig_item< inherit $ct class_t$  >>],
                    [<:class_sig_item< inherit $ct class_tt$ >>]
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
           let cases, methods, methods_sig, methods_sig_t = split4 match_cases in
	   let type_methods, type_methods_sig = split (get_type_methods ()) in
           let methods          = flatten methods       in
           let methods_sig      = flatten methods_sig   in
	   let methods_sig_t    = flatten methods_sig_t in
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
           let proto_class_def  = <:str_item< class type $list:[class_info false (class_tt name) proto_class_type]$ >> in
           let proto_class_decl = <:sig_item< class type $list:[class_info false (class_tt name) proto_class_type]$ >> in 
           let class_def  = <:str_item< class $list:[class_info true (class_t name) class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info true (class_t name) class_type]$ >> in 
	   (H.P.constr (H.P.id name) catype, H.E.record [generic_cata, H.E.id (cata name)]),
           (H.P.id (cata name), (H.E.func (map H.P.id args) (local_defs_and_then (H.E.match_e subj cases)))),
           <:sig_item< value $name$ : $catype$ >>,
           (proto_class_def, proto_class_decl),
           (let env, protos, defs, edecls, pdecls, decls = split6 (map get_derived_classes derived) in
            class_def, env@protos, defs, class_decl::edecls@pdecls@decls 
	   )
      ) 
      d
  in
  let tuples, defs, decls, classes, derived_classes = split5 defs in
  let pnames, tnames                                = split tuples in
  let class_defs, class_decls                       = split classes in
  let derived_class_defs, derived_class_decls       = 
    let class_defs, protos, defs, class_decls = split4 derived_classes in
    class_defs@(flatten protos)@(flatten defs), flatten class_decls
  in
  let cata_def  = <:str_item< value $list:[H.P.tuple pnames, H.E.letrec defs (H.E.tuple tnames)]$ >> in
  let type_def  = <:str_item< type $list:t$ >> in
  let type_decl = <:sig_item< type $list:t$ >> in
  <:str_item< declare $list:type_def::class_defs@[cata_def]@derived_class_defs$ end >>,
  <:sig_item< declare $list:type_decl::class_decls@decls@derived_class_decls$ end >> 
    
