open GT

module Asttypes6  = Migrate_parsetree.Ast_406.Asttypes
module Parsetree6 = Migrate_parsetree.Ast_406.Parsetree
module Location6 = Migrate_parsetree.Ast_406.Location
module Longident6 = Migrate_parsetree.Ast_406.Longident

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
  (* class ['self] html_t_t fself =
   *   object inherit  [unit,'self,View.viewer] t_t end
   * let rec html_t subj =
   *   GT.fix0 (fun self -> gcata_t ((new html_t_t) self) ()) subj *)
  class ['self] fmt_t_t fself = object
    inherit  [Format.formatter, 'self, unit] t_t
    method do_t = fmt_location
  end
  let rec fmt_t fmt subj =
    GT.fix0 (fun self -> gcata_t ((new fmt_t_t) self) ) fmt subj
  let t =
    {
      GT.gcata = gcata_t;
      GT.plugins = (object (* method html = html_t *) method fmt = fmt_t end)
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

module MyAsttypes = struct
  type arg_label = Asttypes6.arg_label =
                 | Nolabel
                 | Labelled of string (*  label:T -> ... *)
                 | Optional of string (* ?label:T -> ... *)
  [@@deriving gt ~options:{fmt}]
  type label = string [@@deriving gt ~options:{fmt}]
  type closed_flag = Closed | Open

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  } [@@deriving gt ~options:{fmt}]
end
open MyAsttypes

type attribute = string loc (* * payload *)
and extension = string loc (* * payload *)
and attributes = attribute list
and core_type = (* Parsetree.core_type = *)
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
   }
and core_type_desc = (* Parsetree.core_type_desc = *)
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string
        (* 'a *)
  | Ptyp_arrow of arg_label * core_type * core_type
        (* T1 -> T2       Simple
           ~l:T1 -> T2    Labelled
           ?l:T1 -> T2    Optional
         *)
  | Ptyp_tuple of core_type list
        (* T1 * ... * Tn
           Invariant: n >= 2
        *)
  | Ptyp_constr of Longident.t loc * core_type list
        (* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
         *)
  | Ptyp_object of object_field list * closed_flag
        (* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
         *)
  | Ptyp_class of Longident.t loc * core_type list
        (* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
         *)
  | Ptyp_alias of core_type * string
        (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
  | Ptyp_poly of string loc list * core_type
        (* 'a1 ... 'an. T
           Can only appear in the following context:
           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...
           - Under Cfk_virtual for methods (not values).
           - As the core_type of a Pctf_method node.
           - As the core_type of a Pexp_poly node.
           - As the pld_type field of a label_declaration.
           - As a core_type of a Ptyp_object node.
         *)

  | Ptyp_package of package_type
        (* (module S) *)
  | Ptyp_extension of extension
     (* [%id] *)
and package_type = Longident.t loc * (Longident.t loc * core_type) list

and row_field =
  | Rtag of label loc * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )
          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)
          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
  (* [ T ] *)

and object_field =
  | Otag of label loc * attributes * core_type
  | Oinherit of core_type

  [@@deriving gt ~options:{fmt}]
