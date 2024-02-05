@type 'data expr_node =
  | EConst of 'data
  | EAdd of 'data expr *  'data expr
and 'data expr =
  { loc : GT.string ; data : 'data expr_node }
with stateful

let econst c = { loc=""; data = EConst c }
let eadd  l r = { loc=""; data = EAdd (l,r) }

let ((),_) = 
  GT.stateful expr 
    (fun env x  -> (env,print_endline x))
    ()
    (eadd ( econst "1" ) (eadd (econst "2") (econst "3")))
