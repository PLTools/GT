@type expr = 
  Add of expr * expr 
| Mul of expr * expr 
| Int of GT.int
| Var of GT.string with show, foldl, map, html

class show' =
  object inherit @expr[show]
    method c_Var _ _ s = s
  end

class show'' =
  object inherit show'
    method c_Int _ _ i = string_of_int i
  end

class show''' =
  object inherit show''
    method c_Add _ _ x y = x.GT.fx () ^ " + " ^ y.GT.fx ()    
  end

class show'''' =
  let enclose = function 
  | Add (_, _) -> (fun x -> "(" ^ x ^ ")")
  | _ -> (fun x -> x)
  in
  object inherit show'''
    method c_Mul _ _ x y = (enclose x.GT.x (x.GT.fx ())) ^ " * " ^ 
                           (enclose y.GT.x (y.GT.fx ()))
  end

let vars = 
  let module S = Set.Make (String) in
  let module M =
    struct

      class vars = 
        object inherit [S.t] @expr[foldl]
          method c_Var s _ x = S.add x s
        end
 
     let vars e = S.elements (GT.transform(expr) new vars S.empty e)

    end
  in M.vars 
  
let eval =
  let module M =
    struct
      
      class eval =
        object inherit [string -> int, int] @expr
          method c_Var s _ x = s x
          method c_Int _ _ i = i
          method c_Add s _ x y = x.GT.fx s + y.GT.fx s
          method c_Mul s _ x y = x.GT.fx s * y.GT.fx s
        end

      let eval s = GT.transform(expr) new eval s

    end
  in M.eval

let simplify =
  let module M = 
    struct

      class simplify_add =
        let (+) a b =
          match a, b with
	  | Int a, Int b -> Int (a+b)
          | Int a, Add (Int b, c) 
          | Add (Int a, c), Int b -> Add (Int (a+b), c)
          | _, Int _ -> Add (b, a)
          | _ -> Add (a, b)
        in
        object inherit @expr[map]
          method c_Add _ _ x y = x.GT.fx () + y.GT.fx ()
        end
  
      class simplify_mul =
        let ( * ) a b =
          match a, b with
	  | Int a, Int b -> Int (a*b)
          | Int a, Mul (Int b, c) 
          | Mul (Int a, c), Int b -> Mul (Int (a*b), c)
          | _, Int _ -> Mul (b, a)
          | _ -> Mul (a, b)
        in
        object inherit simplify_add
          method c_Mul _ _ x y = x.GT.fx () * y.GT.fx ()
        end
        
      class simplify_all =
        object inherit simplify_mul as super
          method c_Add i it x y = match super#c_Add i it x y with Add (Int 0, a) -> a | x -> x
          method c_Mul i it x y = match super#c_Mul i it x y with Mul (Int 1, a) -> a | x -> x
        end

      let simplify = GT.transform(expr) new simplify_all () 

    end
  in M.simplify
   
let _ =
  let x = Mul (Var "a", Add (Int 1, Var "b")) in
  let y = Add (Int 1, Add (Var "a", Int 3)) in
  let z = Add (Int 0, Mul (Add (Int 2, Int 3), Mul (Var "a", Int 4))) in
  Printf.printf "%s\n" (GT.transform(expr) new @expr[show] () x);
  Printf.printf "%s\n" (GT.transform(expr) new show' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show'' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show'''' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () y);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () z);
  Printf.printf "%d\n" (eval (function "a" -> 1 | "b" -> 2) x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () (simplify y));
  Printf.printf "%s\n" (GT.transform(expr) new show''' () (simplify z));
  Printf.printf "[%s]\n" 
    (GT.transform(GT.list) 
       (fun _ s -> s) 
       new @GT.list[show] 
       () 
       (vars x)
    )

