module Expr =
  struct
  
    generic t = 
    [> 
      | `Var   of [string] 
      | `Const of [int] 
      | `Binop of [int -> int -> int] * [string] * t * t
    ]

    class virtual ['me, 'a, 'b] t_t =
      object (this)
        method virtual m_Var   : 'a -> 'me -> string -> 'b
        method virtual m_Const : 'a -> 'me -> int -> 'b
        method virtual m_Binop : 'a -> 'me -> (int -> int -> int) -> string -> ('a, 'me, 'b) Generic.a -> ('a, 'me, 'b) Generic.a -> 'b
      end

    class ['a] toString =
      object (this)
        inherit ['a, unit, string] t_t
        method m_Var   _ _   s     = s
        method m_Const _ _   n     = string_of_int n
        method m_Binop _ _ _ s x y = "(" ^ (x.Generic.f ()) ^ s ^ (y.Generic.f ()) ^ ")"
      end

    class ['a] eval s =
      object (this)
        inherit ['a, unit, int] t_t
        method m_Var   _ _ x       = s x
        method m_Const _ _ n       = n
        method m_Binop _ _ f _ x y = f (x.Generic.f ()) (y.Generic.f ())
      end

  end

module Stmt =
  struct

    generic 'a t =
    [>
      | `Skip 
      | `Assign of [string] * 'a Expr.t
      | `Read   of [string]
      | `Write  of 'a Expr.t
      | `If     of 'a Expr.t * 'a t * 'a t
      | `While  of 'a Expr.t * 'a t  
      | `Seq    of 'a t * 'a t 
    ]

  end
let _ =
  let e = `Binop ((+), "+", `Const 1, `Var "a") in
  let s = Expr.t.Generic.gcata Generic.apply (new Expr.toString) () e in
  let v = Expr.t.Generic.gcata Generic.apply (new Expr.eval (fun "a" -> 2)) () e in
  Printf.printf "%s\n" s;
  Printf.printf "%d\n" v