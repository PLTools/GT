let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

generic 'a var = [> `Var of [string] ] as 'a deriving show

class ['a, 'v] var_eval = object
  inherit ['a, (string * 'v) list, 'v] @var
  method c_Var s v name = try List.assoc name s with Not_found -> v.GT.x
end

generic 'a lambda = [> 'a var | `Abs of [string] * 'a lambda | `App of 'a lambda * 'a lambda] as 'a deriving show

class ['a, 'v] lambda_eval = object
  inherit ['a, (string * 'v) list, 'v] @lambda
  inherit ['a, 'v] var_eval
  method c_Abs s v name l1 = 
    let s' = gensym () in
   `Abs (s', v.GT.f ((name,`Var s')::s) l1.GT.x)
  method c_App s v l1 l2 =
    let l2' = l2.GT.fx s in
    match l1.GT.fx s with
     `Abs (s, body) ->  v.GT.f [s, l2'] body
    | l1' -> `App (l1', l2')
 end

let eval1 s e = GT.transform(lambda) (new lambda_eval) s e

generic 'a var_expr = [> 'a var | `Num of [int] | `Add of 'a var_expr * 'a var_expr | `Mult of 'a var_expr * 'a var_expr] as 'a
  deriving show 

class ['a] var_expr_eval = object
  inherit ['a, (string * 'a) list, 'a] @var_expr
  inherit ['a, 'a] var_eval
  method c_Num  s v i   = v.GT.x
  method c_Add  s v x y = 
    match x.GT.fx s, y.GT.fx s with
    | `Num x, `Num y -> `Num (x+y)
    | x, y -> `Add (x, y) 
  method c_Mult s v x y =
    match x.GT.fx s, y.GT.fx s with
    | `Num x, `Num y -> `Num (x*y)
    | x, y -> `Mult (x, y)
 end

let eval2 s e = GT.transform(var_expr) (new var_expr_eval) s e

generic 'a expr = [> 'a lambda | 'a var_expr ] as 'a deriving show

class ['a] expr_eval = object
  inherit ['a, 'a] lambda_eval
  inherit ['a] var_expr_eval
end 

let eval3 s e = GT.transform(expr) (new expr_eval) s e

let _ =
  Printf.printf "%s\n" (GT.transform(expr) (new @expr[show]) () (`Add (`Var "x", `Mult (`Num 2, `Var "y")))) 
