#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf
open MLast
open Ploc 

let _ =
  register "eq" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(

        let tags_ctype     = T.app (T.id (tags_t d.name) :: (T.app (T.id d.name :: map T.var d.type_args)) :: map T.var d.type_args) in
        {
          inh_t       = tags_ctype; 
          syn_t       = T.id "bool";
          proper_args = d.type_args; 
          arg_img     = (fun _ -> T.id "bool")
        }, 
        object
	    inherit generator
	  method constr env constr =
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
				 | [n] -> E.app [e; E.app [E.variant (type_tag n); E.id (arg b)]; E.id b]
				 | _::t -> name t
				 in
				 name qname
			     )
			 | Variable (_, a) -> E.app [E.gt_fx (E.id b); E.app [E.variant (arg_tag a      ); E.id (arg b)]]
			 | Self     _      -> E.app [E.gt_fx (E.id b); E.app [E.variant (type_tag d.name); E.id (arg b)]]
			 | Arbitrary _     -> E.uid "true"
		       ]
		)
		(E.uid "true")
		constr.args
	    in
	    E.match_e (E.id env.inh) 
              [P.app [P.variant (type_tag d.name);
                      P.app (((if d.is_polyvar then P.variant else P.uid) constr.constr)::(map (fun (_, a) -> P.id a) args))
                     ], VaVal None, branch; 
               P.wildcard, VaVal None, E.uid "false"
              ]
	end
       )
    )
