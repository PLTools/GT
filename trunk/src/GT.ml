type 'a t = {gcata : 'a}
type ('a, 'b, 'c, 'd) a = {x : 'b; fx : 'a -> 'c; f : 'a -> 'b -> 'c; t : 'd}

let (~:) x = x.x
let transform t = t.gcata

let make  f x p = {x=x; fx=(fun a -> f a x); f=f; t=p}
let apply f a x = f a x

class virtual ['a, 'inh, 'syn] primitive =
  object
    method virtual value : 'inh -> 'a -> 'syn
  end

type pint = int
type int = pint

class virtual ['inh, 'syn] int_t = 
  object (this)
    inherit [int, 'inh, 'syn] primitive
    method t_int = this#value
  end

class show_int_t =
  object
    inherit [unit, string] @int
    method value _ x = string_of_int x
  end

class map_int_t =
  object
    inherit [unit, int] @int
    method value _ x = x
  end

class ['syn] foldl_int_t =
  object
    inherit ['syn, 'syn] @int
    method value s _ = s
  end

class ['syn] foldr_int_t = 
  object
    inherit ['syn] @foldl[int]
  end

type eq_int_tags = [`tint of int]

class eq_int_t =
  object
    inherit [eq_int_tags, bool] @int
    method value inh x = match inh with `tint y -> x = y 
  end

type comparison = LT | EQ | GT

let chain_compare x f = 
  match x with
  | EQ -> f ()
  | _  -> x

let compare_primitive x y = 
  if x < y 
  then LT
  else if x > y   
       then GT
       else EQ

let poly_tag x =
  let x = Obj.magic x in
  (Obj.magic (if Obj.is_block x then Obj.field x 0 else x) : int)

let vari_tag x =
  if Obj.is_block x then Obj.tag x else Obj.magic x

let compare_poly x y = 
  compare_primitive (poly_tag x) (poly_tag y)

let compare_vari x y =
  let x, y = Obj.magic x, Obj.magic y in
  match compare_primitive (Obj.is_block x) (Obj.is_block y) with
  | EQ -> compare_primitive (vari_tag x) (vari_tag y)
  | c  -> x

class compare_int_t =
  object
    inherit [eq_int_tags, comparison] @int
    method value inh x = match inh with `tint y -> compare_primitive y x
  end

let int : (('inh, 'syn) #@int -> 'inh -> int -> 'syn) t = 
  let int_gcata t inh x = t#value inh x in
  {gcata = int_gcata}

type pstring = string
type string = pstring

class virtual ['inh, 'syn] string_t = 
  object (this)
    inherit [string, 'inh, 'syn] primitive
    method t_string = this#value
  end

class show_string_t =
  object
    inherit [unit, string] @string
    method value _ x = x
  end

class map_string_t =
  object
    inherit [unit, string] @string
    method value _ x = x
  end

class ['syn] foldl_string_t =
  object
    inherit ['syn, 'syn] @string
    method value s _ = s
  end

class ['syn] foldr_string_t = 
  object
    inherit ['syn] @foldl[string]
  end

type eq_string_tags = [`tstring of string]

class eq_string_t =
  object
    inherit [eq_string_tags, bool] @string
    method value inh x = match inh with `tstring y -> x = y 
  end

class compare_string_t =
  object
    inherit [eq_string_tags, comparison] @string
    method value inh s = match inh with `tstring d -> compare_primitive d s 
  end

let string : (('inh, 'syn) #@string -> 'inh -> string -> 'syn) t = 
  let string_gcata t inh x = t#value inh x in
  {gcata = string_gcata}

type 'a plist = 'a list
type 'a list = 'a plist

class type ['a, 'pa, 'inh, 'syn] list_tt =
  object
    method c_Nil  : 'inh -> ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a -> 'syn
    method c_Cons : 'inh -> ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a ->
                                    ('inh, 'a, 'pa, < a : 'inh -> 'a -> 'pa >) a ->
                                    ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a -> 'syn
    method t_list : ('inh -> 'a -> 'pa) -> 'inh -> 'a list -> 'syn
  end

let list : (('inh -> 'a -> 'pa) -> ('a, 'pa, 'inh, 'syn) #list_tt -> 'inh -> 'a list -> 'syn) t =
  let rec list_gcata fa trans inh subj =
    let rec self = list_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      [] -> trans#c_Nil inh (make self subj tpo)
    | p1::p2 ->
        trans#c_Cons inh (make self subj tpo) (make fa p1 tpo)
          (make self p2 tpo)
  in
  {gcata = list_gcata}

class virtual ['a, 'pa, 'inh, 'syn] list_t =
  object (this)
    method virtual c_Nil :
      'inh -> ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a -> 'syn
    method virtual c_Cons :
      'inh -> ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a ->
        ('inh, 'a, 'pa, < a : 'inh -> 'a -> 'pa >) a ->
        ('inh, 'a list, 'syn, < a : 'inh -> 'a -> 'pa >) a -> 'syn
    method t_list fa = transform list fa this
  end

class ['a] show_list_t =
  object
    inherit ['a, string, unit, string] @list
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = x.fx () ^ (match xs.x with [] -> "" | _ -> ", " ^ xs.fx ())
  end
      
class ['a, 'pa] map_list_t =
  object
    inherit ['a, 'pa, unit, 'pa list] @list
    method c_Nil _ _ = []
    method c_Cons _ _ x xs = x.fx () :: xs.fx ()
  end

class ['a, 'syn] foldl_list_t =
  object
    inherit ['a, 'syn, 'syn, 'syn] @list
    method c_Nil s _ = s
    method c_Cons s _ x xs = xs.fx (x.fx s)
  end

class ['a, 'syn] foldr_list_t =
  object
    inherit ['a, 'syn] @foldl[list]
    method c_Cons s _ x xs = x.fx (xs.fx s)
  end

type 'a eq_list_tags = [`tlist of 'a list | `aa of 'a]

class ['a] eq_list_t =
  object
    inherit ['a, bool, 'a eq_list_tags, bool] @list
    method c_Nil inh subj = 
      match inh with 
      | `tlist [] -> true 
      | _ -> false
    method c_Cons inh subj x xs = 
      match inh with 
      | `tlist (y::ys) -> x.fx (`aa y) && xs.fx (`tlist ys) 
      | _ -> false
  end

class ['a] compare_list_t =
  object
    inherit ['a, comparison, 'a eq_list_tags, comparison] @list
    method c_Nil inh subj =
      match inh with
      | `tlist [] -> EQ
      | `tlist _  -> GT
      | _ -> invalid_arg "type error (should not happen)"
    method c_Cons inh subj x xs =
      match inh with
      | `tlist [] -> LT
      | `tlist (y::ys) -> 
	  (match x.fx (`aa y) with
	  | EQ -> xs.fx (`tlist ys)
	  | c  -> c
	  )
      | _ -> invalid_arg "type error (should not happen)"
  end
