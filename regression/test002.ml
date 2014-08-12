open GT

module Expr =
  struct
  
    @type 'self t = 
    [ 
      | `Var   of string 
      | `Const of int 
      | `Binop of (int -> int -> int) * string * 'self * 'self 
    ] 

    class ['a] toString =
      object
        inherit ['a, unit, string, unit, string] t_t
        method c_Var   _ _   s     = s
        method c_Const _ _   n     = string_of_int n
        method c_Binop _ _ _ s x y = "(" ^ (x.fx ()) ^ s ^ (y.fx ()) ^ ")"
      end

    class ['a] eval s =
      object
        inherit ['a, unit, int, unit, int] t_t
        method c_Var   _ _ x       = s x
        method c_Const _ _ n       = n
        method c_Binop _ _ f _ x y = f (x.fx ()) (y.fx ())
      end

  end

let _ =
  let rec toString s e = transform(Expr.t) toString (new Expr.toString) () e in
  let rec eval s i e = transform(Expr.t) (eval s) (new Expr.eval s) i e in
  let e = `Binop ((+), "+", `Const 1, `Var "a") in

  let s = toString () e in
  let v = eval (fun "a" -> 2) () e in
  Printf.printf "%s\n" s;
  Printf.printf "%d\n" v
