#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf
open MLast
open Ploc 

exception Found of int

let _ =
  register "eq" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(
        {
          inh_t       = `Poly (T.app (T.id d.name :: map T.var d.type_args) , fun x -> T.var x); 
          syn_t       = T.id "bool";
          proper_args = d.type_args; 
          arg_img     = (fun _ -> T.id "bool")
        }, 
        object
	  inherit generator
	  method constr env constr =
	    let arg_num a =
	      let n = ref 0 in
	      try
	        ignore (map (fun arg -> let i = !n in incr n; if a = arg then Pervasives.raise (Found i) else 0) d.type_args);
		Pervasives.raise Not_found
	      with Found i -> i
	    in
	    let gen    = name_generator (map fst constr.args) in
	    let args   = map (fun a -> a, gen#generate a) (map fst constr.args) in
	    let arg  a = assoc a args in
	    let branch = 
	      fold_left
		(fun acc (b, typ) ->
		  E.app [E.lid "&&";
			 acc;
			 match typ with
			 | Instance (_, args, qname) ->
			     (match env.trait "eq" typ with
			     | None   -> E.uid "true"
			     | Some e -> 
				 let rec name = function
				 | [n] -> E.app [e; E.app [E.variant type_tag; E.id (arg b)]; E.id b]
				 | _::t -> name t
				 in
				 name qname
			     )
			 | Variable (_, a) -> E.app [E.gt_fx (E.id b); E.app [E.variant (arg_tag (arg_num a) d.name); E.id (arg b)]]
			 | Self     _      -> E.app [E.gt_fx (E.id b); E.app [E.variant type_tag; E.id (arg b)]]
			 | Arbitrary _     -> E.uid "true"
		       ]
		)
		(E.uid "true")
		constr.args
	    in
	    E.match_e (E.id env.inh) 
              [P.app [P.variant type_tag;
                      P.app (((if d.is_polyvar then P.variant else P.uid) constr.constr)::(map (fun (_, a) -> P.id a) args))
                     ], VaVal None, branch; 
               P.wildcard, VaVal None, E.uid "false"
              ]
	end
       )
    )
