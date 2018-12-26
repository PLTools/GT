@type 'info expr_node =
  | EConst of GT.int
  | EAdd of 'info expr * 'info expr
and 'info expr =
  { info : 'info ; node : 'info expr_node }
with show,gmap



let e1 = {info="asdf"; node=EConst 19}
let e2 = {info="x"; node= EAdd ({info="y";node=EConst 20}, {info="z";node=EConst 40})}
let () =
  print_endline @@ show_expr (GT.lift GT.id) e1;
  print_endline @@ show_expr (GT.lift GT.id) @@ gmap_expr (GT.lift ((^)"__")) e1;
  print_endline @@ show_expr (GT.lift GT.id) e2;
  print_endline @@ show_expr (GT.lift GT.id) @@ gmap_expr (GT.lift ((^)"__")) e2;
  ()
