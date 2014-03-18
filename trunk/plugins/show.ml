#load "q_MLast.cmo";;

open Pa_gt.Plugin

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = `Mono (T.id "unit"); 
          syn_t       = T.id "string";
          proper_args = d.type_args; 
          arg_img     = (fun _ -> T.id "string")
        }, 
        object
	  inherit generator
	  method constr env constr =
            let concat x y = E.app [E.lid "^"; x; y] in
            concat 
              (snd 
		 (List.fold_left 
                    (fun (first, expr as acc) arg ->
                      let append e = 
			false, concat expr (if first then e else concat (E.str ", ") e)
                      in
                      match arg with                     
		      | arg, (Variable _ | Self _) -> 
                          append (E.app [E.gt_fx (E.lid arg); E.unit])
                      | arg, typ -> 
			  (match env.trait "show" typ with
			  | Some e -> append (E.app [e; E.unit; E.lid arg])
			  | None   -> acc
			  )
                    )         
                    (true, E.str ((if d.is_polyvar then "`" else "") ^ constr.constr ^ " ("))
                    constr.args 
		 )
              )
              (E.str ")")
	end
     )
    )
    
