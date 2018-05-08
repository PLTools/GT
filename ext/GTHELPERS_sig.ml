module type S = sig
module Located :
  sig
    type t
    val mk : loc:'a -> 'b -> 'b
  end
type loc = Located.t

type type_arg
val named_type_arg : string -> type_arg

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
    val from_caml: Ppxlib.expression -> t
    val ident : loc:loc -> string -> t
    val of_longident: loc:loc -> Ppxlib.longident -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a

    val unit : loc:loc -> t
    val uid  : loc:loc -> string -> t
    val lid  : loc:loc -> string -> t
    val int_const : loc:loc -> int -> t
    val record : loc:loc -> (Ppxlib.longident * t) list -> t
    val app : loc:loc -> t -> t -> t
    val app_list : loc:loc -> t -> t list -> t
    val acc      : loc:loc -> t -> t -> t
    val acc_list : loc:loc -> t -> t list -> t
    val fun_ : loc:loc -> Pat.t -> t -> t
    val fun_list : loc:loc -> Pat.t list -> t -> t
    val match_ : loc:loc -> t -> (Pat.t * t option * t) list -> t
    val object_ : loc:loc -> Pat.t * Cf.t list -> t
    val send : loc:loc -> t -> string -> t
    val new_ : loc:loc -> Ppxlib.longident -> t
  end
and Typ :
  sig
    type t
    val from_caml: Ppxlib.core_type -> t
    val use_tdecl: Ppxlib.type_declaration -> t
    val of_type_arg: type_arg -> t
    val of_longident : loc:loc -> Ppxlib.longident -> t
    val var : loc:loc -> string -> t
    val ident : loc:loc -> string -> t
    val constr : loc:loc -> Ppxlib.longident -> t list -> t
    val object_ : loc:loc -> Ppxlib.closed_flag -> (string * t) list -> t
    val arrow : loc:loc -> t -> t -> t
    val chain_arrow : loc:loc -> t list -> t
    val variant : loc:loc -> ?is_open:bool -> Ppxlib.row_field list -> t
  end
and Cf :
  sig
    type t
    val method_concrete : loc:loc -> string -> Exp.t -> t
    val method_virtual : loc:loc -> string -> Typ.t -> t
    val inherit_: loc:loc -> ?as_:(string option) -> Cl.t -> t
  end
and Cty : sig
  type t
  val arrow: loc:loc -> Typ.t -> t -> t
  val constr:loc:loc -> Ppxlib.longident -> type_arg list -> t
end
and Ctf : (* class_sig_item *)
  sig
    type t
    val inherit_: loc:loc -> Cty.t -> t
    val method_ : loc:loc -> string -> ?virt:bool -> Typ.t -> t
  end
and Str :
  sig
    type t
    val of_tdecls : loc:loc -> Typ.t list -> t
    val single_value : loc:loc -> Pat.t -> Exp.t -> t
    val values: loc:loc -> Vb.t list -> t
    val class_single :
      loc:loc ->
      name:string ->
      ?virt:bool ->
      ?wrap:(Cl.t -> Cl.t) ->
      params:type_arg list -> Cf.t list -> t
    val tdecl : loc:loc -> name:string -> params:string list -> Typ.t -> t
  end
and Cl :    (* class_expr *)
  sig
    type t
    val fun_:     loc:loc -> Pat.t -> t -> t
    val fun_list: loc:loc -> Pat.t list -> t -> t
    val constr :  loc:loc -> Longident.t -> type_arg list -> t
    val apply  :  loc:loc -> t -> Exp.t list -> t
  end
and Sig :
  sig
    type t
    val value : loc:loc -> name:string -> Typ.t -> t
    val class_: loc:loc -> name:string ->
      params: type_arg list ->
      ?virt:bool ->
      ?wrap:(Cty.t -> Cty.t) ->
      Ctf.t list ->
      t
  end
and Vb :
  sig
    type t
  end

val value_binding: loc:loc -> pat:Pat.t -> expr:Exp.t -> Vb.t

val prepare_param_triples :
  loc:loc -> ?extra:string list ->
  ?inh:(loc:loc -> string -> type_arg) ->
  ?syn:(loc:loc -> string -> type_arg) ->
  ?default_inh: type_arg ->
  ?default_syn: type_arg ->
  string list -> type_arg list

(* val invariantize : string option list -> type_var list *)

end
