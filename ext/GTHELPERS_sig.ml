module type S = sig
module Located :
  sig
    type t
    val mk : loc:'a -> 'b -> 'b
  end
type loc = Located.t
type type_var
module Pat :
  sig
    type t
    val any : loc:loc -> unit -> t
    val var : loc:loc -> string -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a
    val of_longident : loc:loc -> Ppxlib.longident -> t
    val constr : loc:loc -> string -> t list -> t
  end
val class_structure : self:'a -> fields:'b -> 'a * 'b
module rec Exp :
  sig
    type t
    val ident : loc:loc -> string -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a
    val record : loc:loc -> (Ppxlib.longident * t) list -> t
    val object_ : loc:loc -> Pat.t * Cf.t list -> t
    val send : loc:loc -> t -> string -> t
    val app : loc:loc -> t -> t -> t
    val app_list : loc:loc -> t -> t list -> t
    val match_ : loc:loc -> t -> (Pat.t * t option * t) list -> t
    val fun_ : loc:loc -> Pat.t -> t -> t
    val fun_list : loc:loc -> Pat.t list -> t -> t
  end
and Typ :
  sig
    type t
    val of_longident : loc:loc -> Ppxlib.longident -> t
    val var : loc:loc -> string -> t
    val constr : loc:loc -> Ppxlib.longident -> t list -> t
    val object_ : loc:loc -> Ppxlib.closed_flag -> (string * t) list -> t
    val arrow : loc:loc -> t -> t -> t
    val chain_arrow : loc:loc -> t list -> t
  end
and Cf :
  sig
    type t
    val method_concrete : loc:loc -> string -> Exp.t -> t
    val method_virtual : loc:loc -> string -> Typ.t -> t
  end
module Str :
  sig
    type t
    val of_tdecls : loc:loc -> Typ.t list -> t
    val single_value : loc:loc -> Pat.t -> Exp.t -> t
    val class_single :
      loc:loc ->
      name:string ->
      params:type_var list -> Cf.t list -> t
    val tdecl : loc:loc -> name:string -> params:string list -> Typ.t -> t
  end
module Sig :
  sig
    type t
    val value : loc:loc -> name:string -> Typ.t -> t
  end

val prepare_param_triples :
  loc:'a -> ?extra:string list -> string list -> string option list

val invariantize : string option list -> type_var list

end
