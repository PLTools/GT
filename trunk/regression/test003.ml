(*
module GenericExpr =
  struct

    generic t = 
      Var   of string 
    | Const of int 
    | Add   of [t] * [t]
    | Sub   of [t] * [t] deriving show

    let show = GT.transform(t) (new @t[show]) () 

    class eval = object
      inherit [string -> int, int] @t
      method m_Var   s _ v = s v
      method m_Const _ _ i = i
      method m_Add   s _ x y = (x.GT.fx s) + (y.GT.fx s)
      method m_Sub   s _ x y = (x.GT.fx s) - (y.GT.fx s)
    end

    class better_show = object
      inherit @t[show]
      method m_Var _ _ s = s
    end

  end

module NaivePoly =
  struct

  end
*)

generic 'a ident = [> `Var of string ] as 'a

class ['a, 'v] ident_eval = object 
  inherit ['a, string -> 'v, 'v] @ident      
  method m_Var s _ x = s x
end

generic 'a arith = [> `Add of ['a arith] * ['a arith] | `Sub of ['a arith] * ['a arith]] as 'a

class ['a, 'b] arith_eval = object
  inherit ['a, 'b, int] @arith
  method m_Add inh _ x y = x.GT.fx inh + y.GT.fx inh
  method m_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh
end

generic 'a expr = [> 'a ident | 'a arith ] as 'a 

class ['a] expr_eval = object
  inherit ['a, int] ident_eval
  inherit ['a, string -> int] arith_eval
end

let _ =
  Printf.printf "%d\n" (GT.transform(expr) (new expr_eval) (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))
