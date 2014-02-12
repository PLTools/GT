@type 'a ident = [> `Var of [string] ] as 'a

class ['v] ident_eval = object 
  inherit [string -> 'v, 'v] @ident      
  method c_Var s _ x = s x
end

@type 'a arith = [> `Add of 'a arith * 'a arith | `Sub of 'a arith * 'a arith] as 'a

class ['b] arith_eval = object
  inherit ['b, int] @arith
  method c_Add inh _ x y = x.GT.fx inh + y.GT.fx inh
  method c_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh
end

@type 'a expr = [> 'a ident | 'a arith ] as 'a 

class expr_eval = object
  inherit [int] ident_eval
  inherit [string -> int] arith_eval
end

let _ =
  Printf.printf "%d\n" (expr.transform_expr (new expr_eval) (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))
