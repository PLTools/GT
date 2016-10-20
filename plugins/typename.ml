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
        object
	  inherit generator
	  method record      env fields    = E.str ""
	  method tuple       env elems     = E.str ""
	  method constructor env name args = E.str ""
	end
     )
    )
    
