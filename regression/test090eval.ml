module Abs = struct
  @type ('name, 'term) t = [`Abs of 'name * 'term ] with show,gmap,eval;;
  class ['term, 'term2, 'extra] de_bruijn ft =
    object
      inherit [string, unit, 'term, 'term2, 'env, 'extra] eval_t_t
          (fun _ -> assert false)
          (fun _ _ -> ())
          ft
      constraint 'env = string list
      constraint 'extra = [> (unit, 'term2) t]
      method c_Abs env name term = `Abs ((), ft (name :: env) term)
    end
end

module Lam = struct
  @type ('name, 'lam) t = [`App of 'lam * 'lam | `Var of 'name] with show,eval
end
module Let = struct
  @type ('name, 'term) t = [`Let of 'name * 'term * 'term] with show,eval
  class ['me, 'term2, 'extra] de_bruijn ft = object
    inherit [string, unit, 'me, 'term2, 'env, 'extra] eval_t_t
        (fun _ -> assert false)
        (fun _ _ -> ())
        ft
    constraint 'env = string list
    constraint 'extra = [> (unit, 'term2) t]
    method c_Let env name bnd term = `Let ((), ft env bnd,  ft (name :: env) term)
  end
end

module LetRec = struct
  @type ('name, 'term) t = [`LetRec of 'name * 'term * 'term] with show,eval;;

  class ['me, 'me2, 'extra] de_bruijn ft = object
    inherit [string, unit, 'me, 'me2, 'env, 'extra] eval_t_t
        (fun _ -> assert false)
        (fun _ _ -> ())
        ft
    constraint 'env = string list
    constraint 'extra = [> (unit, 'term2) t]
    method c_LetRec env name bnd term =
      let env' = name :: env in
      `LetRec ((), ft env' bnd,  ft env' term)
  end
end
;;

@type ('n, 'b) t =
   [ ('n, ('n, 'b) t) Lam.t
   | ('b, ('n, 'b) t) Abs.t
   | ('b, ('n, 'b) t) Let.t
   | ('b, ('n, 'b) t) LetRec.t
   ] with show,eval
;;
@type named    = (GT.string, GT.string) t with show;;
@type nameless = (GT.int, GT.unit) t with show;;

class ['extra] de_bruijn fself = object
  inherit [string, int, string, unit, 'env, 'extra] eval_t_t
      fself (fun _ _ -> 100500) (fun _ _ -> ())
  constraint 'env = string list
  inherit [named, nameless, 'extra]    Abs.de_bruijn fself
  inherit [named, nameless, 'extra]    Let.de_bruijn fself
  inherit [named, nameless, 'extra] LetRec.de_bruijn fself
end

let convert term =
  GT.fix0 (fun f -> GT.transform(t) (new de_bruijn f)) [] term

let _ =
  let l = `App (`Abs ("x", `Var "x"), `Abs ("y", `Var "y")) in
  Printf.printf "Original: %s\n" (GT.show(named) l);
  let m = `App (`Abs ((), `Var 1), `Abs ((), `Var 2)) in
  Printf.printf "Nameless : %s\n" (GT.show(nameless) m);
  Printf.printf "Converted: %s\n" (GT.show(nameless) @@ convert l);
  Printf.printf "Converted: %s\n" @@ GT.show(nameless) @@ convert @@
  `Let    ("z", `Abs ("x", `Var "x"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))));
  Printf.printf "Converted: %s\n" @@ GT.show(nameless) @@ convert @@
  `LetRec ("z", `App (`Abs ("x", `Var "x"), `Var "z"),
           `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))))
