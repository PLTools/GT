(* Remake from the paper `Code reuse through polymorphic variants`
 * https://www.math.nagoya-u.ac.jp/~garrigue/papers/mixev3.04.ml.txt
 * *)

(* TODO: decide are manual open type annotations really required? *)

let rec to_string = function
| `Var   s     -> s
| `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
| `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
| `Num   i     -> string_of_int i
| `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
| `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

(* environment: our inherited attribute *)
type 'a env = (string * 'a) list
type var = [`Var of string ] [@@deriving gt]

class ['v, 'extra] var_eval (fself: _ -> [> var ] -> _) = object
  inherit [ 'v env, 'v, 'extra] var_t
  method c_Var inh _ name = try List.assoc name inh with Not_found -> `Var name
end

type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a] [@@deriving gt]


class ['a, 'v, 'extra] lambda_eval (fself: _ -> [> 'v lambda ] -> _) fa = object(self)
  inherit [ 'v env, 'a,     'v
          , 'v env, 'extra, 'v
          ] lambda_t
  inherit ['v, 'extra] var_eval fself
  constraint 'a = [> _ lambda ]

  method c_Abs s _ name l1 =
    let s' = gensym () in
    `Abs (s', fself ((name, `Var s')::s) l1)

  method c_App s _ l1 l2 =
    (* Call by value *)
    let l2' = fself s l2 in
    match fself s l1 with
    | `Abs (s, body) -> fa [s, l2'] body
    | l1' -> `App (l1', l2')
 end

(* 2nd extension *)
type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a] [@@deriving gt]

class ['a, 'v, 'extra] var_expr_eval (fself:  _ -> [> 'v var_expr] -> _) = object
  inherit [ 'v env, 'a, 'v
          , 'v env, 'v, 'extra] var_expr_t
  inherit ['v, 'extra] var_eval fself

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

(* 3rd extension *)
type 'a expr = ['a lambda | 'a var_expr] [@@deriving gt]

class ['a, 'v] expr_eval fself = object
  inherit [ 'v env, 'a, 'v, 'v env, 'v, 'extra] expr_t
  inherit ['a, 'v, 'extra] lambda_eval fself fself
  inherit ['a, 'v, 'extra] var_expr_eval fself
end

let rec eval3 s e =
  (* TODO: define generic catamrhisms to use GT.transform_gc instead of GT.fix0 *)
  GT.fix0 (fun self -> gcata_expr (new expr_eval self) ) s e

let _ =
  (* x+2*y when x=5 and y=6 *)
  Printf.printf "%s\n" @@ to_string @@
  eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y")));

  (* (fun y -> x+y)x when x = 1 *)
  Printf.printf "%s\n" @@ to_string @@
  eval3 ["x", `Num 1] (`App (`Abs ("y", `Add (`Var "x", `Var "y")), `Var "x") );
  ()


(*
module A = struct
  type var = [`Var of GT.string ] [@@deriving gt ~options:{eval}]

  class ['v, 'extra] var_eval (fself: _ -> [> var ] -> _) = object
    inherit [ 'v env, 'v, 'extra] var_t
    method c_Var inh _ name = try List.assoc name inh with Not_found -> `Var name
  end

  type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a]
  [@@deriving gt ~options:{eval}]


  class ['a, 'v, 'extra] lambda_eval (fself: _ -> [> 'v lambda ] -> _) fa = object(self)
    inherit [ 'v env, 'a,     'v
            , 'v env, 'extra, 'v
            ] lambda_t
    inherit ['v, 'extra] var_eval fself
    constraint 'a = [> _ lambda ]

    method c_Abs s _ name l1 =
      let s' = gensym () in
      `Abs (s', fself ((name, `Var s')::s) l1)

    method c_App s _ l1 l2 =
      (* Call by value *)
      let l2' = fself s l2 in
      match fself s l1 with
      | `Abs (s, body) -> fa [s, l2'] body
      | l1' -> `App (l1', l2')
  end

  (* 2nd extension *)
  type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a] [@@deriving gt]

  class ['a, 'v, 'extra] var_expr_eval (fself:  _ -> [> 'v var_expr] -> _) = object
    inherit [ 'v env, 'a, 'v
            , 'v env, 'v, 'extra] var_expr_t
    inherit ['v, 'extra] var_eval fself

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

  (* 3rd extension *)
  type 'a expr = ['a lambda | 'a var_expr] [@@deriving gt]

  class ['a, 'v] expr_eval fself = object
    inherit [ 'v env, 'a, 'v, 'v env, 'v, 'extra] expr_t
    inherit ['a, 'v, 'extra] lambda_eval fself fself
    inherit ['a, 'v, 'extra] var_expr_eval fself
  end

let rec eval3 s e =
  (* TODO: define generic catamrhisms to use GT.transform_gc instead of GT.fix0 *)
  GT.fix0 (fun self -> gcata_expr (new expr_eval self) ) s e

let _ =
  (* x+2*y when x=5 and y=6 *)
  Printf.printf "%s\n" @@ to_string @@
  eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y")));

  (* (fun y -> x+y)x when x = 1 *)
  Printf.printf "%s\n" @@ to_string @@
  eval3 ["x", `Num 1] (`App (`Abs ("y", `Add (`Var "x", `Var "y")), `Var "x") );
  ()

end
*)
