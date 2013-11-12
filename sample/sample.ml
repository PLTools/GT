module Expr =
  struct

    type t = 
      Var   of string 
    | Const of int 
    | Add   of t * t
    | Sub   of t * t

    let rec show = function
    | Var   s -> "Var (" ^ s ^ ")"
    | Const i -> "Const (" ^ string_of_int i ^ ")"
    | Add  (x, y) -> "Add (" ^ show x ^ ", " ^ show y ^ ")"
    | Sub  (x, y) -> "Sub (" ^ show x ^ ", " ^ show y ^ ")"

    let bitter_show = function
    | Var s -> s
    | x -> show x

    let rec eval s = function
    | Var   v     -> s v
    | Const i     -> i
    | Add  (x, y) -> eval s x + eval s y 
    | Sub  (x, y) -> eval s x - eval s y 

    module ObjectEncoded =
      struct

        let rec generic_show t e = 
          let self = generic_show t in
          match e with
          | Var   s -> t#m_Var s
          | Const i -> t#m_Const i
          | Add  (x, y) -> t#m_Add self x y
          | Sub  (x, y) -> t#m_Sub self x y

        class show_t = object
          method m_Var   s = "Var (" ^ s ^ ")"
          method m_Const i = "Const (" ^ string_of_int i ^ ")"
          method m_Add (f : t -> string) x y = "Add (" ^ f x ^ ", " ^ f y ^ ")"
          method m_Sub (f : t -> string) x y = "Sub (" ^ f x ^ ", " ^ f y ^ ")"
        end

        let show e = generic_show (new show_t) e
        let another_show e = generic_show (object 
                                             inherit show_t
                                             method m_Var s = s
                                           end) e
      end

  end

module GenericExpr =
  struct

    generic t = 
      Var   of [string] 
    | Const of [int] 
    | Add   of t * t
    | Sub   of t * t deriving show

    let show = GT.transform(t) (new @t[show]) () 

    class eval = object
      inherit [string -> int, int] @t
      method m_Var   s _ v = s v
      method m_Const _ _ i = i
      method m_Add   s _ x y = (x.GT.fx s) + (y.GT.fx s)
      method m_Sub   s _ x y = (x.GT.fx s) - (y.GT.fx s)
    end

    class better_show = object
      inherit @t[show]
      method m_Var _ _ s = s
    end

  end

module NaivePoly =
  struct

    type ident = [ `Var of string ]

    let rec transform_ident t inh id =
      let self = transform_ident t in
      match id with `Var v -> t#m_Var inh id 

    class virtual ['inh, 'syn] ident_t = object
       method virtual m_Var : 'inh -> string -> 'syn
    end
 
    class ['v] ident_eval = object
      inherit [string -> 'v, 'v] ident_t
      method m_Var s id = s id
    end
      
    type 'a arith = [`Add of 'a * 'a | `Sub of 'a * 'a]

    let rec transform_arith fa t inh e =
      let self = transform_arith fa t in
      match e with 
      | `Add (x, y) -> t#m_Add self inh x y
      | `Sub (x, y) -> t#m_Sub self inh x y
     
    class virtual ['a, 'ta, 'inh, 'syn] arith_t = object
      method virtual m_Add : ('a -> 'inh -> 'ta) -> 'inh -> 'a arith -> 'a arith -> 'syn
      method virtual m_Sub : ('a -> 'inh -> 'ta) -> 'inh -> 'a arith -> 'a arith -> 'syn
    end
 
    class ['a, 'inh] arith_eval = object
      inherit ['a, int, 'inh, int] arith_t
      method m_Add self inh x y = self inh x + self inh y
      method m_Sub self inh x y = self inh x - self inh y
    end

    type expr = [ ident | expr arith ]

    class virtual ['inh, 'syn] expr_t = object
      inherit ['inh, 'syn] ident_t
      inherit [expr, 'syn, 'inh, 'syn] arith_t
    end

    (* this would not compile: 
    class expr_eval = object
      inherit [int] ident_eval
      inherit [expr, int] arith_eval 
    end
    *)

  end

module Poly =
  struct

    generic 'a ident = [> `Var of [string] ] as 'a

    class ['a, 'v] ident_eval = object 
      inherit ['a, string -> 'v, 'v] @ident      
      method m_Var s _ x = s x
    end

    generic 'a arith = [> `Add of 'a arith * 'a arith | `Sub of 'a arith * 'a arith] as 'a

    class ['a, 'b] arith_eval = object
      inherit ['a, 'b, int] @arith
      method m_Add inh _ x y = x.GT.fx inh + y.GT.fx inh
      method m_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh
    end

    generic 'a expr = [> 'a ident | 'a arith ] as 'a 

    class ['a] expr_eval = object
      inherit ['a, int] ident_eval
      inherit ['a, string -> int] arith_eval
    end

  end

let _ =
  Printf.(
    Expr.(
      Printf.printf "%s\n" (show (Add (Var "a", Const 1)));
      Printf.printf "%s\n" (bitter_show (Add (Var "a", Const 1)))
    );
    GenericExpr.(
      Printf.printf "%s\n" (show (Add (Var "a", Const 1)));
      Printf.printf "%s\n" (GT.transform(t) (new better_show) () (Add (Var "a", Const 1)));
    );
    Poly.(
     printf "%d\n" (GT.transform(expr) (new expr_eval) (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y")))
    )
  )

