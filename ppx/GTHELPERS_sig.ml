module type S = sig

val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val nolabelize : 'a list -> (Ppxlib.arg_label * 'a) list
val invariantize : 'a list -> ('a * Ppxlib.variance) list
val lid : ?loc:Location.t -> 'a -> 'a Location.loc
val mknoloc : 'a -> 'a Location.loc
val pexp_pair :
  ?loc:Location.t ->
  Ppxlib.expression -> Ppxlib.expression -> Ppxlib.expression
val const_string : ?wtf:string -> string -> Ppxlib.constant
module Pat :
  sig
    val any : ?loc:Location.t -> unit -> Ppxlib.pattern
    val constraint_ :
      ?loc:Location.t -> Ppxlib.pattern -> Ppxlib.core_type -> Ppxlib.pattern
    val construct :
      ?loc:Location.t ->
      Ppxlib.longident -> Ppxlib.pattern option -> Ppxlib.pattern
    val variant :
      ?loc:Location.t -> string -> Ppxlib.pattern option -> Ppxlib.pattern
    val tuple : ?loc:Location.t -> Ppxlib.pattern list -> Ppxlib.pattern
    val var : ?loc:Location.t -> string -> Ppxlib.pattern
    val of_string : ?loc:Location.t -> string -> Ppxlib.pattern
    val sprintf :
      ?loc:Location.t -> ('a, unit, string, Ppxlib.pattern) format4 -> 'a
    val alias : ?loc:Location.t -> Ppxlib.pattern -> string -> Ppxlib.pattern
    val type_ :
      ?loc:Location.t -> Ppxlib__.Import.longident_loc -> Ppxlib.pattern
    val record :
      ?loc:Location.t ->
      ?flag:Ppxlib.closed_flag ->
      (Ppxlib__.Import.longident_loc * Ppxlib.pattern) list -> Ppxlib.pattern
  end
module Exp :
  sig
    val apply :
      ?loc:Location.t ->
      Ppxlib.expression ->
      (Ppxlib.arg_label * Ppxlib.expression) list -> Ppxlib.expression
    val apply_nolabeled :
      ?loc:Location.t ->
      Ppxlib.expression -> Ppxlib.expression list -> Ppxlib.expression
    val apply1 :
      ?loc:Location.t ->
      ?label:Ppxlib.arg_label ->
      Ppxlib.expression -> Ppxlib.expression -> Ppxlib.expression
    val case :
      ?guard:Ppxlib.expression ->
      Ppxlib.pattern -> Ppxlib.expression -> Ppxlib.case
    val constant : ?loc:Location.t -> Ppxlib.constant -> Ppxlib.expression
    val construct :
      ?loc:Location.t ->
      Ppxlib.longident -> Ppxlib.expression option -> Ppxlib.expression
    val variant :
      ?loc:Location.t ->
      string -> Ppxlib.expression option -> Ppxlib.expression
    val record :
      ?loc:Location.t ->
      ?with_what:Ppxlib.expression ->
      (Ppxlib.longident * Ppxlib.expression) list -> Ppxlib.expression
    val field :
      ?loc:Location.t ->
      Ppxlib.expression -> Ppxlib__.Import.longident_loc -> Ppxlib.expression
    val ident : ?loc:Location.t -> string -> Ppxlib.expression
    val ident_of_long :
      ?loc:Location.t -> Ppxlib.longident -> Ppxlib.expression
    val sprintf :
      ?loc:Location.t -> ('a, unit, string, Ppxlib.expression) format4 -> 'a
    val make_list :
      ?loc:Location.t -> Ppxlib.expression list -> Ppxlib.expression
    val match_ :
      ?loc:Location.t ->
      Ppxlib.expression -> Ppxlib.case list -> Ppxlib.expression
    val new_ :
      ?loc:Location.t -> Ppxlib__.Import.longident_loc -> Ppxlib.expression
    val object_ :
      ?loc:Location.t -> Ppxlib.class_structure -> Ppxlib.expression
    val tuple :
      ?loc:Location.t -> Ppxlib.expression list -> Ppxlib.expression
    val maybe_tuple :
      ?loc:Location.t -> Ppxlib.expression list -> Ppxlib.expression option
    val fun_ :
      ?loc:Location.t ->
      Ppxlib.arg_label ->
      Ppxlib.expression option ->
      Ppxlib.pattern -> Ppxlib.expression -> Ppxlib.expression
    val fun_list :
      ?loc:Location.t ->
      args:Ppxlib.pattern list -> Ppxlib.expression -> Ppxlib.expression
    val send :
      ?loc:Location.t ->
      Ppxlib.expression -> string Location.loc -> Ppxlib.expression
    val letmodule :
      ?loc:Location.t ->
      string Location.loc ->
      Ppxlib.module_expr -> Ppxlib.expression -> Ppxlib.expression
    val pack_with_constraint :
      ?loc:Location.t ->
      Ppxlib.module_expr ->
      Ppxlib__.Import.longident_loc -> Ppxlib.expression
  end
module Cl :
  sig
    val mk :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.class_expr_desc -> Ppxlib.class_expr
    val attr :
      Ppxlib.class_expr -> Ppxlib.Parsetree.attribute -> Ppxlib.class_expr
    val constraint_ :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.class_expr -> Ppxlib.class_type -> Ppxlib.class_expr
    val extension :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.Parsetree.extension -> Ppxlib.class_expr
    val fun_list :
      Ppxlib.pattern list -> Ppxlib.class_expr -> Ppxlib.class_expr
    val apply :
      Ppxlib.class_expr ->
      (Ppxlib.arg_label * Ppxlib.expression) list -> Ppxlib.class_expr
    val fun_ :
      ?loc:Location.t ->
      Ppxlib.arg_label ->
      Ppxlib.expression option ->
      Ppxlib.pattern -> Ppxlib.class_expr -> Ppxlib.class_expr
    val constr :
      ?loc:Location.t ->
      Ppxlib.longident -> Ppxlib.core_type list -> Ppxlib.class_expr
    val structure :
      ?loc:Location.t -> Ppxlib.class_structure -> Ppxlib.class_expr
    val let_ :
      ?loc:Location.t ->
      ?flg:Ppxlib.rec_flag ->
      Ppxlib.value_binding list -> Ppxlib.class_expr -> Ppxlib.class_expr
  end
module Typ :
  sig
    val mk :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.core_type_desc -> Ppxlib.core_type
    val attr :
      Ppxlib.core_type -> Ppxlib.Parsetree.attribute -> Ppxlib.core_type
    val any :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs -> unit -> Ppxlib.core_type
    val var :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs -> string -> Ppxlib.core_type
    val tuple :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.core_type list -> Ppxlib.core_type
    val alias :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.core_type -> string -> Ppxlib.core_type
    val variant :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.row_field list ->
      Ppxlib.closed_flag -> string list option -> Ppxlib.core_type
    val poly :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib_ast__Ast_helper.str list -> Ppxlib.core_type -> Ppxlib.core_type
    val extension :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.Parsetree.extension -> Ppxlib.core_type
    val force_poly : Ppxlib.core_type -> Ppxlib.core_type
    val varify_constructors :
      Ppxlib_ast__Ast_helper.str list -> Ppxlib.core_type -> Ppxlib.core_type
    val ground : ?loc:Location.t -> string -> Ppxlib.core_type
    val constr :
      ?loc:Location.t ->
      Ppxlib__.Import.longident_loc ->
      Ppxlib.core_type list -> Ppxlib.core_type
    val object_ :
      ?loc:Location.t ->
      Ppxlib.closed_flag ->
      (string * Ppxlib.core_type) list -> Ppxlib.core_type
    val package :
      ?loc:Location.t -> Ppxlib__.Import.longident_loc -> Ppxlib.core_type
    val arrow :
      ?loc:Location.t ->
      ?label:Ppxlib.arg_label ->
      Ppxlib.core_type -> Ppxlib.core_type -> Ppxlib.core_type
    val class_ :
      ?loc:Location.t ->
      Ppxlib.longident -> Ppxlib.core_type list -> Ppxlib.core_type
    val chain_arrow :
      ?loc:Location.t -> Ppxlib.core_type list -> Ppxlib.core_type
  end
module Str :
  sig
    val mk :
      ?loc:Location.t -> Ppxlib.structure_item_desc -> Ppxlib.structure_item
    val eval :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__.Import.Parsetree.attributes ->
      Ppxlib.expression -> Ppxlib.structure_item
    val primitive :
      ?loc:Location.t -> Ppxlib.value_description -> Ppxlib.structure_item
    val type_ :
      ?loc:Location.t ->
      Ppxlib.rec_flag ->
      Ppxlib.type_declaration list -> Ppxlib.structure_item
    val type_extension :
      ?loc:Location.t -> Ppxlib.type_extension -> Ppxlib.structure_item
    val exception_ :
      ?loc:Location.t ->
      Ppxlib.extension_constructor -> Ppxlib.structure_item
    val module_ :
      ?loc:Location.t -> Ppxlib.module_binding -> Ppxlib.structure_item
    val rec_module :
      ?loc:Location.t -> Ppxlib.module_binding list -> Ppxlib.structure_item
    val modtype :
      ?loc:Location.t ->
      Ppxlib.module_type_declaration -> Ppxlib.structure_item
    val open_ :
      ?loc:Location.t -> Ppxlib.open_description -> Ppxlib.structure_item
    val class_ :
      ?loc:Location.t ->
      Ppxlib_ast__.Import.Parsetree.class_declaration list ->
      Ppxlib.structure_item
    val class_type :
      ?loc:Location.t ->
      Ppxlib_ast__.Import.Parsetree.class_type_declaration list ->
      Ppxlib.structure_item
    val include_ :
      ?loc:Location.t ->
      Ppxlib_ast__.Import.Parsetree.include_declaration ->
      Ppxlib.structure_item
    val extension :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.Parsetree.extension -> Ppxlib.structure_item
    val attribute :
      ?loc:Location.t -> Ppxlib.Parsetree.attribute -> Ppxlib.structure_item
    val text : Ppxlib_ast__.Docstrings.text -> Ppxlib.structure_item list
    val single_class :
      ?loc:Location.t ->
      ?virt:Ppxlib.virtual_flag ->
      ?pat:Ppxlib.pattern ->
      ?wrap:(Ppxlib.class_expr -> Ppxlib.class_expr) ->
      name:string ->
      params:(Ppxlib.core_type * Ppxlib.variance) list ->
      Ppxlib.class_field list -> Ppxlib.structure_item
    val class_single :
      ?loc:Location.t ->
      ?virt:Ppxlib.virtual_flag ->
      ?pat:Ppxlib.pattern ->
      ?wrap:(Ppxlib.class_expr -> Ppxlib.class_expr) ->
      name:string ->
      params:(Ppxlib.core_type * Ppxlib.variance) list ->
      Ppxlib.class_field list -> Ppxlib.structure_item
    val value :
      ?loc:Location.t ->
      ?flag:Ppxlib.rec_flag ->
      Ppxlib.value_binding list -> Ppxlib.structure_item
    val single_value :
      ?loc:Location.t ->
      ?flag:Ppxlib.rec_flag ->
      Ppxlib.pattern -> Ppxlib.expression -> Ppxlib.structure_item
  end
module Sig :
  sig
    val mk :
      ?loc:Location.t -> Ppxlib.signature_item_desc -> Ppxlib.signature_item
    val type_ :
      ?loc:Location.t ->
      Ppxlib.rec_flag ->
      Ppxlib.type_declaration list -> Ppxlib.signature_item
    val type_extension :
      ?loc:Location.t -> Ppxlib.type_extension -> Ppxlib.signature_item
    val exception_ :
      ?loc:Location.t ->
      Ppxlib.extension_constructor -> Ppxlib.signature_item
    val module_ :
      ?loc:Location.t -> Ppxlib.module_declaration -> Ppxlib.signature_item
    val rec_module :
      ?loc:Location.t ->
      Ppxlib.module_declaration list -> Ppxlib.signature_item
    val modtype :
      ?loc:Location.t ->
      Ppxlib.module_type_declaration -> Ppxlib.signature_item
    val open_ :
      ?loc:Location.t -> Ppxlib.open_description -> Ppxlib.signature_item
    val include_ :
      ?loc:Location.t ->
      Ppxlib_ast__.Import.Parsetree.include_description ->
      Ppxlib.signature_item
    val class_type :
      ?loc:Location.t ->
      Ppxlib_ast__.Import.Parsetree.class_type_declaration list ->
      Ppxlib.signature_item
    val extension :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.Parsetree.extension -> Ppxlib.signature_item
    val attribute :
      ?loc:Location.t -> Ppxlib.Parsetree.attribute -> Ppxlib.signature_item
    val text : Ppxlib_ast__.Docstrings.text -> Ppxlib.signature_item list
    val class_ :
      ?loc:Location.t ->
      ?virt:Ppxlib.virtual_flag ->
      ?wrap:(Ppxlib.class_type -> Ppxlib.class_type) ->
      name:string ->
      params:(Ppxlib.core_type * Ppxlib.variance) list ->
      Ppxlib.class_type_field list -> Ppxlib.signature_item
    val value :
      ?loc:Location.t ->
      ?prim:string list ->
      name:string -> Ppxlib.core_type -> Ppxlib.signature_item
  end
module Cf :
  sig
    val constraint_ :
      ?loc:Location.t ->
      Ppxlib.core_type -> Ppxlib.core_type -> Ppxlib.class_field
    val inherit_ :
      ?loc:Location.t ->
      ?flg:Ppxlib.override_flag ->
      ?as_:string Location.loc -> Ppxlib.class_expr -> Ppxlib.class_field
    val method_ :
      ?loc:Location.t ->
      string ->
      ?flg:Ppxlib.private_flag ->
      Ppxlib.class_field_kind -> Ppxlib.class_field
    val method_concrete :
      ?loc:Location.t ->
      string ->
      ?flg:Ppxlib.private_flag ->
      ?over_flg:Ppxlib.override_flag ->
      Ppxlib.expression -> Ppxlib.class_field
  end
module Ctf :
  sig
    val method_ :
      ?loc:Location.t ->
      ?flg:Ppxlib.private_flag ->
      ?virt_flg:Ppxlib.virtual_flag ->
      string -> Ppxlib.core_type -> Ppxlib.class_type_field
    val inherit_ :
      ?loc:Location.t -> Ppxlib.class_type -> Ppxlib.class_type_field
    val constraint_ :
      ?loc:Location.t ->
      Ppxlib.core_type -> Ppxlib.core_type -> Ppxlib.class_type_field
  end
module Cty :
  sig
    val mk :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.class_type_desc -> Ppxlib.class_type
    val attr :
      Ppxlib.class_type -> Ppxlib.Parsetree.attribute -> Ppxlib.class_type
    val constr :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib_ast__Ast_helper.lid ->
      Ppxlib.core_type list -> Ppxlib.class_type
    val signature :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.class_signature -> Ppxlib.class_type
    val extension :
      ?loc:Location.t ->
      ?attrs:Ppxlib_ast__Ast_helper.attrs ->
      Ppxlib.Parsetree.extension -> Ppxlib.class_type
    val arrow :
      ?loc:Location.t ->
      ?label:Ppxlib.arg_label ->
      Ppxlib.core_type -> Ppxlib.class_type -> Ppxlib.class_type
  end
module Cstr :
  sig
    val mk :
      self:Ppxlib.pattern ->
      Ppxlib.class_field list -> Ppxlib.class_structure
  end
val openize_poly : Ppxlib.core_type -> Ppxlib.core_type
val map_type_param_names :
  f:(string -> 'a) -> (Ppxlib.core_type * 'b) list -> 'a list
val affect_longident :
  f:(string -> string) -> Ppxlib.longident -> Ppxlib.longident
val map_longident :
  f:(string -> string) -> Ppxlib.longident -> Ppxlib.longident
val map_core_type :
  onvar:(string -> Ppxlib.core_type) -> Ppxlib.core_type -> Ppxlib.core_type

val compare_core_type : Ppxlib.core_type -> Ppxlib.core_type -> int
val make_gt_a_typ :
  ?loc:Location.t ->
  ?inh:Ppxlib.core_type ->
  ?itself:Ppxlib.core_type ->
  ?syn:Ppxlib.core_type -> ?tpoT:Ppxlib.core_type -> unit -> Ppxlib.core_type
val arr_of_param :
  ?loc:Location.t ->
  ?loc:Location.t ->
  ?inh:(string -> Ppxlib.core_type) ->
  ?syn:(string -> Ppxlib.core_type) ->
  Ppxlib.core_type -> string * 'a list * Ppxlib.core_type
val prepare_param_triples :
  ?loc:Location.t ->
  ?extra:(unit -> Ppxlib.core_type list) ->
  ?normal:(loc:Location.t -> string -> Ppxlib.core_type) ->
  ?inh:(loc:Location.t -> string -> Ppxlib.core_type) ->
  ?syn:(loc:Location.t -> string -> Ppxlib.core_type) ->
  ?default_syn:Ppxlib.core_type ->
  ?default_inh:Ppxlib.core_type ->
  ?middle:Ppxlib.core_type list ->
  (Ppxlib.core_type * 'a) list -> Ppxlib.core_type list
val using_type :
  typename:string -> Ppxlib.type_declaration -> Ppxlib.core_type
val inh_syn_ts : ?loc:Location.t -> unit -> Ppxlib.core_type list
val are_the_same : Ppxlib.core_type -> Ppxlib.type_declaration -> bool
val visit_typedecl :
  loc:'a ->
  ?onrecord:(Ppxlib.label_declaration list -> 'b) ->
  ?onmanifest:(Ppxlib.core_type -> 'b) ->
  ?onvariant:(Ppxlib.constructor_declaration list -> 'b) ->
  Ppxlib.type_declaration -> 'b
val make_new_names : ?prefix:string -> int -> string list
val unfold_tuple : Ppxlib.core_type -> Ppxlib.core_type list
val prepare_patt_match_poly :
  loc:Location.t ->
  Ppxlib.expression ->
  Ppxlib.row_field list ->
  string list option ->
  onrow:(string -> (string * Ppxlib.core_type) list -> Ppxlib.expression) ->
  onlabel:(string -> string -> Ppxlib.expression) ->
  oninherit:(Ppxlib.core_type list ->
             Ppxlib.longident -> string -> Ppxlib.expression) ->
  Ppxlib.expression
val prepare_patt_match :
  loc:Location.t ->
  Ppxlib.expression ->
  [< `Algebraic of Ppxlib.constructor_declaration list | `PolyVar of 'a ] ->
  (Ppxlib.constructor_declaration -> string list -> Ppxlib.expression) ->
  Ppxlib.expression
val with_constr_typ :
  Ppxlib.core_type ->
  ok:(Ppxlib.longident Location.loc -> Ppxlib.core_type list -> 'a) ->
  fail:(unit -> 'a) -> 'a
val constr_of_tuple :
  ?loc:Location.t -> Ppxlib.core_type list -> Ppxlib.core_type
val is_polyvariant : Ppxlib.core_type -> bool
val is_polyvariant_tdecl : Ppxlib.type_declaration -> bool

end
