type ('a, 'b) t = {gcata : 'a; gcata_ext : 'b}
type ('a, 'b, 'c, 'd) a = {x : 'b; fx : 'a -> 'c; f : 'a -> 'b -> 'c; t : 'd}

let (~:) x = x.x
let transform t = t.gcata

let make  f x p = {x=x; fx=(fun a -> f a x); f=f; t=p}
let apply f a x = f a x

let list = 
  let rec gcata ext t fa acc l =
    let tpo = object method e = fa end in
    let self = gcata ext t fa in
    match l with
    | []    -> t#m_Nil  acc l 
    | h::tl -> t#m_Cons acc l (make fa h tpo) (make self tl tpo)
  in 
  {gcata = gcata; gcata_ext = gcata}

class virtual ['e, 'a, 'b] list_t =
  object (self)
    method virtual m_Nil  : 'a -> 'e list -> 'b
    method virtual m_Cons : 'a -> 'e list -> ('a, 'e, 'b, <e : 'e -> 'a -> 'b>) a -> ('a, 'e list, 'b, <e : 'e -> 'a -> 'b>) a -> 'b
  end

let int =
  let gcata ext t acc n = t#int n acc n in
  {gcata = gcata; gcata_ext = gcata}

let sum f g = fun ext acc x -> f (fun self acc s -> g (fun _ acc x -> ext self acc x) acc s) acc x
let (++) = sum
      
module Plugin =
  struct

    open MLast

    type properties = {
      inh     : ctyp;
      syn     : ctyp;
      arg_img : string -> ctyp;
    }

    type type_descriptor = {
      is_polyvar : bool;
      is_open    : [`Yes of string | `No];
      type_args  : string list;
      name       : string;
      default    : properties;
    }

    type constructor = {
      name : string;
      inh  : string;
      subj : string;
      args : (string * [ `Processed of string list * string list | `Var of string | `Protected of ctyp ]) list;
    }
      
    type t = type_descriptor -> properties * (constructor -> expr)

    module M = Map.Make (String)
    
    let m : t M.t ref = ref M.empty

    let register name t =
      if not (M.mem name !m) 
      then m := M.add name t !m

    let get name =
      if not (M.mem name !m) then None else Some (M.find name !m)

  end