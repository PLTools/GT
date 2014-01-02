generic ident = [ `Var of [string] ] 

class ['v] ident_eval = object 
  inherit [string -> 'v, 'v] @ident      
  method c_Var s _ x = s x
end


generic 'a arith = [ `Add of 'a * 'a | `Sub of 'a * 'a] 


class ['b] arith_eval = object
  inherit [int, 'b, int] @arith
  method c_Add inh _ x y = x.GT.fx inh + y.GT.fx inh
  method c_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh
end

generic 'a expr = [ ident | 'a arith ] 

class expr_eval = object
  inherit [int] ident_eval
  inherit [string -> int] arith_eval
end

let _ =
  let rec eval f x = expr.transform_expr eval (new expr_eval) f x in
  Printf.printf "%d\n" (eval (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))

