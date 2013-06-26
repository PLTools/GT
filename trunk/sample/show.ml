#load "q_MLast.cmo";;

open Pa_gt.Plugin

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh = T.id "unit"; 
          syn = T.id "string"; 
          arg_img = (fun _ -> T.id "string")
        }, 
        (fun constr -> 
           let concat x y = E.app [E.lid "^"; x; y] in
           concat 
             (snd 
               (List.fold_left 
                  (fun (first, expr as acc) arg ->
                     let append e = 
                       false, concat expr (if first then e else concat (E.str ", ") e)
                     in
                     match arg with
                     | arg, (`Variable _ | `Specific _) -> 
                        append (E.app [E.fx (E.lid arg); E.unit])
                     | arg, `Generic ctyp -> 
                        match ctyp with
                        | <:ctyp< $lid:tname$ >> -> 
                          (match tname with
                           | "int"    -> append (E.app [E.lid "string_of_int"; E.lid arg])
                           | "string" -> append (E.lid arg)
                           | _        -> acc
                          )
                        | _ ->  acc
                  )         
                  (true, E.str (constr.constr ^ " ("))
                  constr.args 
               )
             )
             (E.str ")")
        )
       )
    )