type 'a t = {gcata : 'a}
type ('a, 'b, 'c, 'd) a = {x : 'b; fx : 'a -> 'c; f : 'a -> 'b -> 'c; t : 'd}

let (~:) x = x.x
let transform t = t.gcata

let make  f x p = {x=x; fx=(fun a -> f a x); f=f; t=p}
let apply f a x = f a x

class virtual ['a, 'inh, 'syn] primitive =
  object
    method virtual value : 'a -> 'inh -> 'syn
  end

type pint = int
type int = pint

class virtual ['inh, 'syn] int_t = 
  object(this)
    inherit [int, 'inh, 'syn] primitive
    method t_int inh x = this#value x inh
  end

class show_int_t =
  object
    inherit [unit, string] @int
    method value x _ = string_of_int x
  end

let int : (('inh, 'syn) #@int -> 'inh -> int -> 'syn) t = 
  let int_gcata t inh x = t#value x inh in
  {gcata = int_gcata}

type pstring = string
type string = pstring

class virtual ['inh, 'syn] string_t = 
  object(this)
    inherit [string, 'inh, 'syn] primitive
    method t_string inh x = this#value x inh
  end

class show_string_t =
  object
    inherit [unit, string] @string
    method value x _ = x
  end

let string : (('inh, 'syn) #@string -> 'inh -> string -> 'syn) t = 
  let string_gcata t inh x = t#value x inh in
  {gcata = string_gcata}


(*
let list = 
  let rec gcata ext t fa acc l =
    let tpo = object method e = fa end in
    let self = gcata ext t fa in
    match l with
    | []    -> t#m_Nil  acc l 
    | h::tl -> t#m_Cons acc l (make fa h tpo) (make self tl tpo)
  in  
  {gcata = gcata; gcata_ext = gcata(*; traits=object end*)}

class virtual ['e, 'a, 'b] list_t =
  object (self)
    method virtual m_Nil  : 'a -> 'e list -> 'b
    method virtual m_Cons : 'a -> 'e list -> ('a, 'e, 'b, <e : 'e -> 'a -> 'b>) a -> ('a, 'e list, 'b, <e : 'e -> 'a -> 'b>) a -> 'b
  end

let int =
  let gcata ext t acc n = t#int n acc n in
  {gcata = gcata; gcata_ext = gcata(*; traits=object end*)}
*)
let sum f g = fun ext acc x -> f (fun self acc s -> g (fun _ acc x -> ext self acc x) acc s) acc x
let (++) = sum
      
