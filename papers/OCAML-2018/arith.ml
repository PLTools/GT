open GT

@type expr =
| Var   of string
| Add   of expr * expr
| Mul   of expr * expr
| Div   of expr * expr
| Const of int with show, gmap

class simplifier f =
  object inherit [_] @expr[gmap] f
    method c_Div _ x y =
      match f x, f y with
      | Const x, Const y -> Const (x / y)
      | x      , Const 1 -> x
      | x      , y       -> Div (x, y)
    method c_Mul _ x y =
      match f x, f y with
      | Const x, Const y        -> Const (x * y)
      | Const 0, _ | _, Const 0 -> Const 0
      | Const 1, y              -> y
      | x, Const 1              -> x
      | x, y                    -> Mul (x, y)
    method c_Add _ x y =
      match f x, f y with
      | Const x, Const y -> Const (x + y)
      | Const 0, y       -> y
      | x, Const 0       -> x
      | x, y             -> Add (x, y)
  end

class ns_simplifier f =
  object inherit simplifier f 
    method c_Mul _ x y =
      match f x with
      | Const 0 -> Const 0
      | Const 1 -> f y
      | Const x -> (match f y with                      
                    | Const y -> Const (x * y)
                    | y       -> Mul   (Const x, y)
                   )
      | x       -> (match f y with
                    | Const 0 -> Const 0
                    | Const 1 -> x
                    | y       -> Mul (x, y)
                   )
  end
    
let simplify e =
  fix0 (fun f -> transform(expr) ( new simplifier f) ()) e 
           
let ns_simplify e = fix0 (fun f -> transform(expr) (new ns_simplifier f) ()) e

let substitute st e =
  fix0
    (fun f ->
       transform(expr)
         (object inherit [_] @expr[gmap] f
            method c_Var _ x = Const (st x)
          end)
         ()) e

 let eval st e = let Const n = simplify @@ substitute st e in n
                                                                
 let _ =
   let e = Mul (Add (Var "a", Const 3), Add (Const 5, Var "b")) in
   Printf.printf "Original           : %s\n" (show(expr) e);
   Printf.printf "Simplified         : %s\n" (show(expr) @@ simplify e);
   Printf.printf "Substitute         : %s\n" (show(expr) @@ substitute (function "a" -> 0 | "b" -> 1) e);
   Printf.printf "Substitute+simplify: %s\n" (show(expr) @@ simplify @@ substitute (function "a" -> 0 | "b" -> 1) e);
   Printf.printf "Eval               : %d\n" (eval  (function "a" -> 0 | "b" -> 1) e);
   let e = Mul (Mul (Var "a", Const 0), Div (Const 1, Const 0)) in
   Printf.printf "Simplified         : %s\n" (show(expr) @@ ns_simplify e)
   
