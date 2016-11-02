#load "q_MLast.cmo";;

open Pa_gt.Plugin
open Printf
open List

let _ =
  register "typename" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = T.id "unit"; 
          syn_t       = T.id "string";
          proper_args = d.type_args; 
          fixed_inh   = Some <:expr< () >>;
          sname       = (fun _ -> T.id "string");
          iname       = (fun _ -> T.id "unit")
        }, 
        let tname env =
          match d.type_args with
	  | [] -> E.str d.name
	  | _  -> 
	      let args = 
	        E.app [
	          <:expr< String.concat >>;
	          E.str ", ";              
	          List.fold_right 
	            (fun a expr ->
	               E.app [
	                  E.id "::";
		          E.app [E.gt_tp (E.id env.subj) a; <:expr< () >>];
		          expr
		      ]
	           ) 
	           d.type_args 	
		   (E.id "[]")
	        ]
	      in args
	in
        object
	  inherit generator
	  method record      env fields    = tname env
	  method tuple       env elems     = tname env
	  method constructor env name args = tname env
	end
     )
    )
    
