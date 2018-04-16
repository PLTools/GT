(* let rec to_string = function
 * | `Var   s     -> s
 * | `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
 * | `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
 * | `Num   i     -> string_of_int i
 * | `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
 * | `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"
 *
 * let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n;; *)

type var = [`Var of string | `Foo of int * string ] [@@deriving gt]

type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a]  [@@deriving gt]

(* class ['v, 'extra] var_eval = object
 *   inherit [(string * 'v) list, 'v, 'extra] class_var
 *   method c_Var inh name = try List.assoc name inh with Not_found -> `Var name
 * end
 *
 *
 * class ['a, 'v, 'extra] lambda_eval fa = object(self)
 *
 *   inherit ['a, (string * 'v) list, 'sa
 *           , (string * 'v) list, 'v
 *           , 'extra] class_lambda
 *   inherit ['v, 'extra] var_eval
 *   constraint 'a = [> 'a lambda ]
 *   constraint 'sa = 'v
 *   method c_Abs s name l1 =
 *     assert false
 *
 *     (\* let s' = gensym () in
 *      * `Abs (s', gcata_lambda self ((name, `Var s')::s) l1) *\)
 *
 *   (\* method c_App s l1 l2 =
 *    *   let l2' = gcata_lambda self s l2 in
 *    *   match gcata_lambda self s l1 with
 *    *   | `Abs (s, body) -> fa [s, l2'] body
 *    *   | l1' -> `App (l1', l2') *\)
 *  end *)
