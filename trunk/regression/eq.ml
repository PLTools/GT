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
        let tags_type_name = "eq_" ^ d.name ^ "_tags" in
        let tags_ctype     = T.app (T.id tags_type_name :: map T.var d.type_args) in
        let arg_tag  a     = "a" ^ a in
        let type_tag a     = "t" ^ a in
	let tags           = (type_tag d.name) :: (map arg_tag d.type_args) in
        let gen            = name_generator (d.name::d.type_args@tags) in 
        let tags_def       =
	  {tdNam = VaVal (loc, VaVal tags_type_name);
           tdPrm = VaVal (map (fun name -> VaVal (Some name), None) d.type_args);
           tdPrv = VaVal false;
           tdDef = T.eq_variant (
                               (T.pv_constr (type_tag d.name) [T.app (T.id d.name :: map T.var d.type_args)])::
                               (map (fun a -> T.pv_constr (arg_tag a) [T.var a]) d.type_args)
                             ); 
           tdCon = VaVal []
          }         
	in
        {
          inh_t       = tags_ctype; 
          syn_t       = T.id "bool";
          proper_args = d.type_args; 
          arg_img     = (fun _ -> T.id "bool")
        }, 
        object
	  method header     = [<:str_item< type $list:[tags_def]$ >>]
	  method header_sig = [<:sig_item< type $list:[tags_def]$ >>]
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
			 | Arbitrary ctyp ->
			     (match env.trait "eq" ctyp with
			     | None   -> E.uid "true"
			     | Some e -> 
				 let rec name = function
				   | <:ctyp< $t$ _ >> | <:ctyp< _ . $t$ >> -> name t
				   | <:ctyp< $lid:n$ >> -> E.app [e; E.app [E.variant (type_tag n); E.id (arg b)]; E.id b]
				   | _ -> E.uid "true"
				 in
				 name ctyp
			     )
			 | Variable (_, a) -> E.app [E.gt_fx (E.id b); E.app [E.variant (arg_tag a      ); E.id (arg b)]]
			 | Instance _      -> E.app [E.gt_fx (E.id b); E.app [E.variant (type_tag d.name); E.id (arg b)]]
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
