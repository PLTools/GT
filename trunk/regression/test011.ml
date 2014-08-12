let rec to_string = function
| `Var   s     -> s
| `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
| `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
| `Num   i     -> string_of_int i
| `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
| `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n;;

@type var = [`Var of GT.string] with show

class ['v] var_eval = object
  inherit [(string * 'v) list, 'v] @var
  method c_Var s v name = try List.assoc name s with Not_found -> `Var name 
end

@type 'a lambda = [var | `Abs of GT.string * 'a | `App of 'a * 'a] with show

class ['a, 'v] lambda_eval = object
  inherit ['a, (string * 'v) list, 'v, (string * 'v) list, 'v] @lambda
  inherit ['v] var_eval 
  method c_Abs s v name l1 = 
    let s' = gensym () in
   `Abs (s', l1.GT.fx ((name, `Var s')::s))
  method c_App s v l1 l2 = 
    let l2' = l2.GT.fx s in
    match l1.GT.fx s with
     `Abs (s, body) -> v.GT.t#a [s, l2'] body  
    | l1' -> `App (l1', l2')
 end

let rec eval1 s e = GT.transform(lambda) eval1 (new lambda_eval) s e;;

@type 'a var_expr = [var | `Num of GT.int | `Add of 'a * 'a | `Mult of 'a * 'a] with show

class ['a, 'v] var_expr_eval = object
  inherit ['a, (string * 'v) list, 'v, (string * 'v) list, 'v] @var_expr
  inherit ['v] var_eval
  method c_Num  s v i   = `Num i
  method c_Add  s v x y = 
    match x.GT.fx s, y.GT.fx s with
    | `Num x, `Num y -> `Num (x+y)
    | x, y -> `Add (x, y) 
  method c_Mult s v x y =
    match x.GT.fx s, y.GT.fx s with
    | `Num x, `Num y -> `Num (x*y)
    | x, y -> `Mult (x, y)
 end

let rec eval2 s e = GT.transform(var_expr) eval2 (new var_expr_eval) s e;;

@type 'a expr = ['a lambda | 'a var_expr] with show

class ['a, 'v] expr_eval = object
  inherit ['a, (string * 'v) list, 'v, (string * 'v) list, 'v] @expr
  inherit ['a, 'v] lambda_eval
  inherit ['a, 'v] var_expr_eval
end 

let rec eval3 s e = GT.transform(expr) eval3 (new expr_eval) s e

let _ =
  Printf.printf "%s\n" (to_string (eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y"))))) 

