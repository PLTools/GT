open GT

module Expr =
  struct
  
    generic 'self t = 
    [> 
      | `Var   of [string] 
      | `Const of [int] 
      | `Binop of [int -> int -> int] * [string] * 'self t * 'self t
    ] as 'self

    class ['a] toString =
      object (this)
        inherit ['a, unit, string] t_t
        method c_Var   _ _   s     = s
        method c_Const _ _   n     = string_of_int n
        method c_Binop _ _ _ s x y = "(" ^ (x.fx ()) ^ s ^ (y.fx ()) ^ ")"
      end

    class ['a] eval s =
      object (this)
        inherit ['a, unit, int] t_t
        method c_Var   _ _ x       = s x
        method c_Const _ _ n       = n
        method c_Binop _ _ f _ x y = f (x.fx ()) (y.fx ())
      end

  end
(*
module Stmt =
  struct

    generic ('self, 'a) t = 
    [>
      | `Skip 
      | `Assign of [string] * 'a Expr.t
      | `Read   of [string]
      | `Write  of 'a Expr.t
      | `If     of 'a Expr.t * ('self, 'a) t * ('self, 'a) t
      | `While  of 'a Expr.t * ('self, 'a) t  
      | `Seq    of ('self, 'a) t * ('self, 'a) t 
    ] as 'self

  end
*)
let _ =
  let e = `Binop ((+), "+", `Const 1, `Var "a") in
  let s = Expr.t.Expr.transform_t (new Expr.toString) () e in
  let v = Expr.t.Expr.transform_t (new Expr.eval (fun "a" -> 2)) () e in
  Printf.printf "%s\n" s;
  Printf.printf "%d\n" v
