(* Remake from the paper `Code reuse through polymorphic variants`
 * https://www.math.nagoya-u.ac.jp/~garrigue/papers/mixev3.04.ml.txt
 * *)

(* TODO: decide are mannual open type annotations required *)

let rec to_string = function
| `Var   s     -> s
| `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
| `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
| `Num   i     -> string_of_int i
| `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
| `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

type var = [`Var of string ] [@@deriving gt]

class ['v, 'extra] var_eval (fself: _ -> [> var ] -> _) = object
  inherit [(string * 'v) list, 'v, 'extra] var_t
  method c_Var inh _ name = try List.assoc name inh with Not_found -> `Var name
end

type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a] [@@deriving gt]

class ['a, 'v, 'extra] lambda_eval (fself: _ -> [> 'v lambda ] -> _) fa = object(self)
  inherit [ (string * 'v) list, 'a,     'sa
          , (string * 'v) list, 'extra, 'v
          ] lambda_t
  inherit ['v, 'extra] var_eval fself
  constraint 'a = [> _ lambda ]
  constraint 'sa = 'v

  method c_Abs s _ name (l1: 'a) =
    let s' = gensym () in
    `Abs (s', fself ((name, `Var s')::s) l1)

  method c_App s _ l1 l2 =
    let l2' = fself s l2 in
    match fself s l1 with
    | `Abs (s, body) -> fa [s, l2'] body
    | l1' -> `App (l1', l2')
 end

(* let rec eval1 s e =
 *   GT.fix0(fun self -> (gcata_lambda (new lambda_eval self self)) ) s e
 *
 * let () =
 *   Printf.printf "%s\n%!" @@ to_string @@
 *   eval1 ["x", `Var "y"] (`Abs ("y", `Var "x")) *)

(* 2nd extension *)
type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a] [@@deriving gt]

class ['a, 'v, 'extra] var_expr_eval (fself:  _ -> [> 'v var_expr] -> _) = object
  inherit [ 'inh, 'a, 'v
          , 'inh, 'v, 'extra] var_expr_t
  inherit ['v, 'extra] var_eval fself
  constraint 'inh = (string * 'v) list

  method c_Num  s _ i   = `Num i
  method c_Add  s _ x y =
    match fself s x, fself s y with
    | `Num x, `Num y -> `Num (x+y)
    | x, y -> `Add (x, y)
  method c_Mult s _ x y =
    match fself s x, fself s y with
    | `Num x, `Num y -> `Num (x*y)
    | x, y -> `Mult (x, y)
 end

(* let rec eval2 s e =
 *   GT.fix0 (fun self -> (gcata_var_expr (new var_expr_eval self)) ) s e *)

(* 3rd extension *)
type 'a expr = ['a lambda | 'a var_expr] [@@deriving gt]

class ['a, 'v] expr_eval fself = object
  inherit [(string * 'v) list, 'a, 'v, (string * 'v) list, 'v, 'extra] expr_t
  inherit ['a, 'v, 'extra] lambda_eval fself fself
  inherit ['a, 'v, 'extra] var_expr_eval fself
end

let rec eval3 s e =
  GT.fix0 (fun self -> (gcata_expr (new expr_eval self)) ) s e

let _ =
  Printf.printf "%s\n" @@ to_string @@
  eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y")))
