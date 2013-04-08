type ('a, 'b) t = {gcata : 'a; gcata_ext : 'b}
type ('a, 'b, 'c) a = {x : 'b; f : 'a -> 'c; g : 'a -> 'b -> 'c}

let make f x = {x=x; f=(fun a -> f a x); g=f}
let apply f a x = f a x

let list = 
  let rec gcata ext t fa acc l =
    let self = gcata ext t fa in
    match l with
    | []    -> t#m_Nil  acc l 
    | h::tl -> t#m_Cons acc l (make fa h) (make self tl)
  in 
  {gcata = gcata; gcata_ext = gcata}

class virtual ['e, 'a, 'b] list_t =
  object (self)
    method virtual m_Nil  : 'a -> 'e list -> 'b
    method virtual m_Cons : 'a -> 'e list -> ('a, 'e, 'b) a -> ('a, 'e list, 'b) a -> 'b
  end

let union f g = fun ext acc x -> f (fun self acc s -> g (fun _ acc x -> ext self acc x) acc s) acc x
let (++) = union


(*
f ext acc x
g ext acc x

\ ext acc x -> f (fun self acc x -> g (fun _ acc x -> ext self acc x) acc x) acc x

*)