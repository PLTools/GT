generic 'a ident = [> `Var of [string] ] as 'a

class ['a, 'v] ident_eval = object 
  inherit ['a, string -> 'v, 'v] @ident      
  method c_Var s _ x = s x
end

generic 'a arith = [> `Add of 'a arith * 'a arith | `Sub of 'a arith * 'a arith] as 'a

class ['a, 'b] arith_eval = object
  inherit ['a, 'b, int] @arith
  method c_Add inh _ x y = x.GT.fx inh + y.GT.fx inh
  method c_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh
end

generic 'a expr = [> 'a ident | 'a arith ] as 'a 

class ['a] expr_eval = object
  inherit ['a, int] ident_eval
  inherit ['a, string -> int] arith_eval
end

let _ =
  Printf.printf "%d\n" (GT.transform(expr) (new expr_eval) (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))
