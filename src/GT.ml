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
    inherit ['a, string, unit, string] list_t
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = x.fx () ^ ", " ^ xs.fx ()
  end
      
