open GT

module Location = struct
  (* from Printast module *)
  let fmt_position with_name f l =
    let open Format in
    let open Lexing in
    let fname = if with_name then l.pos_fname else "" in
    if l.pos_lnum = -1
    then fprintf f "%s[%d]" fname l.pos_cnum
    else fprintf f "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol
        (l.pos_cnum - l.pos_bol)

  type      t = [%import:    Location.t]
  (* [@@deriving gt ~options:{show; html}] *)

  let fmt_location f loc =
    let open Format in
    let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
    fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
      (fmt_position p_2nd_name) loc.loc_end;
    if loc.loc_ghost then fprintf f " ghost";

  class virtual ['inh,'self,'syn] t_t =
    object method virtual  do_t : 'inh -> t -> 'syn end
  let gcata_t tr inh subj = tr#do_t inh subj

  class ['self] html_t_t fself = object
    inherit  [unit,'self,View.viewer] t_t
    method do_t () _ = HTML.string "<noloc>"
  end
  let html_t subj =
    GT.fix0 (fun self -> gcata_t ((new html_t_t) self) ()) subj

  class ['self] fmt_t_t fself = object
    inherit  [Format.formatter, 'self, unit] t_t
    method do_t = fmt_location
  end
  let fmt_t fmt subj =
    GT.fix0 (fun self -> gcata_t ((new fmt_t_t) self) ) fmt subj
  let t =
    {
      GT.gcata = gcata_t;
      GT.plugins = (object method html = html_t method fmt = fmt_t end)
    }

  type 'a loc = [%import: 'a Location.loc]
  [@@deriving gt ~options:{fmt}]

  (* class virtual ['ia,'a,'sa,'inh,'self,'syn] loc_t =
   *   object method virtual  do_loc : 'inh -> 'a loc -> 'syn end
   * class ['a,'self] show_loc_t fself  fa =  object
   *   inherit  [unit,'a,string,unit,'self,string] loc_t
   *   method do_loc () { txt; loc } =
   *     Format.sprintf "{  txt=%s; loc=%s; }" (fa txt)
   *       ((fun subj -> (t.GT.plugins)#show subj) loc)
   * end
   * let rec show_loc fa subj =
   *   GT.fix0 (fun self -> gcata_loc ((new show_loc_t) self fa) ()) subj *)

end

module Longident = struct
  type t = [%import: Longident.t] [@@deriving gt ~options:{ fmt; html }]
end

module Asttypes = struct
  type rec_flag       = [%import: Asttypes.rec_flag]       [@@deriving gt ~options:{ fmt; html }]
  type direction_flag = [%import: Asttypes.direction_flag] [@@deriving gt ~options:{ fmt; html }]
  type private_flag   = [%import: Asttypes.private_flag]   [@@deriving gt ~options:{ fmt; html }]
  type mutable_flag   = [%import: Asttypes.mutable_flag]   [@@deriving gt ~options:{ fmt; html }]
  type virtual_flag   = [%import: Asttypes.virtual_flag]   [@@deriving gt ~options:{ fmt; html }]
  type override_flag  = [%import: Asttypes.override_flag]  [@@deriving gt ~options:{ fmt; html }]
  type closed_flag    = [%import: Asttypes.closed_flag]    [@@deriving gt ~options:{ fmt; html }]

  type label = string [@@deriving gt ~options:{ fmt; html }]
  type arg_label =  [%import: Asttypes.arg_label] [@@deriving gt ~options:{ fmt; html }]
  type 'a loc   = [%import: 'a Asttypes.loc]   [@@deriving gt ~options:{ fmt; html }]
  type variance = [%import: Asttypes.variance] [@@deriving gt ~options:{ fmt; html }]
end
open Asttypes

type constant = [%import: Parsetree.constant] [@@deriving gt ~options:{ fmt; html }]


type attribute = [%import: Parsetree.attribute]
and extension = [%import: Parsetree.extension]
and attributes = [%import: Parsetree.attributes]
and payload = [%import: Parsetree.payload]
and core_type = [%import: Parsetree.core_type]
and core_type_desc = [%import: Parsetree.core_type_desc]
and package_type = [%import: Parsetree.package_type]

and row_field = [%import: Parsetree.row_field]

and object_field = [%import: Parsetree.object_field]
and structure = [%import: Parsetree.structure]
and structure_item = [%import: Parsetree.structure_item]
and structure_item_desc = [%import: Parsetree.structure_item_desc]
and value_binding = [%import: Parsetree.value_binding]
and value_description = [%import: Parsetree.value_description]
and type_declaration = [%import: Parsetree.type_declaration]
and type_extension = [%import: Parsetree.type_extension]
and module_binding = [%import: Parsetree.module_binding]
and module_type_declaration = [%import: Parsetree.module_type_declaration]
and open_description = [%import: Parsetree.open_description]
and class_type_declaration = [%import: Parsetree.class_type_declaration]
and class_type = [%import: Parsetree.class_type]
and class_type_desc = [%import: Parsetree.class_type_desc]
and class_signature = [%import: Parsetree.class_signature]
and class_type_field = [%import: Parsetree.class_type_field]
and class_type_field_desc = [%import: Parsetree.class_type_field_desc]
and include_declaration = [%import: Parsetree.include_declaration]
and 'a include_infos = [%import: 'a Parsetree.include_infos]
and module_expr = [%import: Parsetree.module_expr]
and module_expr_desc = [%import: Parsetree.module_expr_desc]
and module_type = [%import: Parsetree.module_type]
and module_type_desc = [%import: Parsetree.module_type_desc]
and class_declaration = [%import: Parsetree.class_declaration]
and 'a class_infos = [%import: 'a Parsetree.class_infos]
and class_expr = [%import: Parsetree.class_expr]
and class_expr_desc = [%import: Parsetree.class_expr_desc]
and class_structure = [%import: Parsetree.class_structure]
and class_field = [%import: Parsetree.class_field]
and class_field_desc = [%import: Parsetree.class_field_desc]
and class_field_kind = [%import: Parsetree.class_field_kind]
and type_kind = [%import: Parsetree.type_kind]
and constructor_declaration = [%import: Parsetree.constructor_declaration]
and constructor_arguments = [%import: Parsetree.constructor_arguments]
and label_declaration = [%import: Parsetree.label_declaration]
and with_constraint = [%import: Parsetree.with_constraint]
and signature = [%import: Parsetree.signature]
and signature_item = [%import: Parsetree.signature_item]
and signature_item_desc = [%import: Parsetree.signature_item_desc]
and module_declaration = [%import: Parsetree.module_declaration]
and include_description = [%import: Parsetree.include_description]
and class_description = [%import: Parsetree.class_description]
and pattern = [%import: Parsetree.pattern]
and pattern_desc = [%import: Parsetree.pattern_desc]
and expression = [%import: Parsetree.expression]
and expression_desc = [%import: Parsetree.expression_desc]
and extension_constructor = [%import: Parsetree.extension_constructor]
and extension_constructor_kind = [%import: Parsetree.extension_constructor_kind]
and case = [%import: Parsetree.case]
[@@deriving gt ~options:{ fmt; html }]
