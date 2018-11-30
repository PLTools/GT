@type ident = [`Var of string]

class ['r] ident_eval = object
  inherit [string -> 'r, _, 'r] @ident
  method c_Var s _ x = s x
end

@type 'a arith = [ `Add of 'a * 'a | `Sub of 'a * 'a] 

class ['a, 'i] arith_eval fa = object
  inherit ['i, 'a, int, 'i, 'self, int] @arith
  method c_Add inh _ x y = (fa inh x) + (fa inh y)
  method c_Sub inh _ x y = (fa inh x) - (fa inh y)
end

@type 'a expr = [ ident | 'a arith ] 

class ['a] expr_eval fa = object(this)
  inherit [string->int, 'a, int, string->int, _, int] @expr
  inherit [int] ident_eval
  inherit ['a, string -> int] arith_eval fa
end

let _ =
  let rec eval f x = GT.transform0(expr) (new expr_eval eval) f x in
  Printf.printf "%d\n" (eval (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))

