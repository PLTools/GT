#load "q_MLast.cmo";;

open Pa_gt.Plugin

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh = tname ["unit"]; 
          syn = tname ["string"]; 
          arg_img = (fun _ -> tname ["string"])
        }, 
        (fun constr -> 
           let concat x y = apply <:expr< $lid:"^"$ >> [x; y] in
           concat 
             (snd 
               (List.fold_left 
                  (fun (first, expr as acc) arg ->
                     let append e = 
                       false, concat expr (if first then e else concat <:expr< $str:", "$ >> e)
                     in
                     match arg with
                     | arg, (`Variable _ | `Specific _) -> 
                        append (apply (fx <:expr< $lid:arg$ >>) [<:expr< () >>])
                     | arg, `Generic ctyp -> 
                        match ctyp with
                        | <:ctyp< $lid:tname$ >> -> 
                          (match tname with
                           | "int"    -> append (apply <:expr< $lid:"string_of_int"$ >> [<:expr< $lid:arg$ >>])
                           | "string" -> append <:expr< $lid:arg$ >>
                           | _        -> acc
                          )
                        | _ ->  acc
                  )         
                  (true, <:expr< $str:constr.constr ^ " ("$ >>)
                  constr.args 
               )
             )
             <:expr< $str:")"$ >>             
        )
       )
    )