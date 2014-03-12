#load "q_MLast.cmo";;

open Pa_gt.Plugin

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = T.id "unit"; 
          syn_t       = T.id "string";
          proper_args = d.type_args; 
          arg_img     = (fun _ -> T.id "string")
        }, 
        (fun env constr -> 
           let concat x y = E.app [E.lid "^"; x; y] in
           concat 
             (snd 
               (List.fold_left 
                  (fun (first, expr as acc) arg ->
                     let append e = 
                       false, concat expr (if first then e else concat (E.str ", ") e)
                     in
                     match arg with                     
                     | arg, Arbitrary ctyp -> 
			 (match env.trait "show" ctyp with
			  | Some e -> append (E.app [e; E.unit; E.lid arg])
			  | None   -> append (E.str "*not supported*")
			 )
(*
                        (match ctyp with
                         | <:ctyp< $lid:tname$ >> -> 
                           (match tname with
                            | "int"    -> append (E.app [E.lid "string_of_int"; E.lid arg])
                            | "string" -> append (E.lid arg)
                            | _        -> acc
                           )
                         | _ ->  acc
			)
*)
		     | arg, _ -> 
                        append (E.app [E.gt_fx (E.lid arg); E.unit])
                  )         
                  (true, E.str (constr.constr ^ " ("))
                  constr.args 
               )
             )
             (E.str ")")
        )
       )
    )
