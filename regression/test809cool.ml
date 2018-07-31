open GT
type longident = [%import: Longident.t]   [@@deriving gt ~options:{show; eval}]

(* module MyAsttypes = struct
 *   type arg_label = Asttypes.arg_label =
 *                  | Nolabel
 *                  | Labelled of string (\*  label:T -> ... *\)
 *                  | Optional of string (\* ?label:T -> ... *\)
 *   type label = string
 *   type closed_flag = Closed | Open
 *
 *   type 'a loc = 'a Location.loc = {
 *     txt : 'a;
 *     loc : Location.t;
 *   }
 * end
 * open MyAsttypes
 *
 * type attribute = string loc (\* * payload *\)
 * and extension = string loc (\* * payload *\)
 * and attributes = attribute list
 * and core_type = (\* Parsetree.core_type = *\)
 *     {
 *      ptyp_desc: core_type_desc;
 *      ptyp_loc: Location.t;
 *      ptyp_attributes: attributes; (\* ... [@id1] [@id2] *\)
 *    }
 * and core_type_desc = (\* Parsetree.core_type_desc = *\)
 *   | Ptyp_any
 *         (\*  _ *\)
 *   | Ptyp_var of string
 *         (\* 'a *\)
 *   | Ptyp_arrow of arg_label * core_type * core_type
 *         (\* T1 -> T2       Simple
 *            ~l:T1 -> T2    Labelled
 *            ?l:T1 -> T2    Optional
 *          *\)
 *   | Ptyp_tuple of core_type list
 *         (\* T1 * ... * Tn
 *            Invariant: n >= 2
 *         *\)
 *   | Ptyp_constr of Longident.t loc * core_type list
 *         (\* tconstr
 *            T tconstr
 *            (T1, ..., Tn) tconstr
 *          *\)
 *   | Ptyp_object of object_field list * closed_flag
 *         (\* < l1:T1; ...; ln:Tn >     (flag = Closed)
 *            < l1:T1; ...; ln:Tn; .. > (flag = Open)
 *          *\)
 *   | Ptyp_class of Longident.t loc * core_type list
 *         (\* #tconstr
 *            T #tconstr
 *            (T1, ..., Tn) #tconstr
 *          *\)
 *   | Ptyp_alias of core_type * string
 *         (\* T as 'a *\)
 *   | Ptyp_variant of row_field list * closed_flag * label list option
 *         (\* [ `A|`B ]         (flag = Closed; labels = None)
 *            [> `A|`B ]        (flag = Open;   labels = None)
 *            [< `A|`B ]        (flag = Closed; labels = Some [])
 *            [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
 *          *\)
 *   | Ptyp_poly of string loc list * core_type
 *         (\* 'a1 ... 'an. T
 *            Can only appear in the following context:
 *            - As the core_type of a Ppat_constraint node corresponding
 *              to a constraint on a let-binding: let x : 'a1 ... 'an. T
 *              = e ...
 *            - Under Cfk_virtual for methods (not values).
 *            - As the core_type of a Pctf_method node.
 *            - As the core_type of a Pexp_poly node.
 *            - As the pld_type field of a label_declaration.
 *            - As a core_type of a Ptyp_object node.
 *          *\)
 *
 *   | Ptyp_package of package_type
 *         (\* (module S) *\)
 *   | Ptyp_extension of extension
 *      (\* [%id] *\)
 * and package_type = Longident.t loc * (Longident.t loc * core_type) list
 *
 * and row_field =
 *   | Rtag of label loc * attributes * bool * core_type list
 *         (\* [`A]                   ( true,  [] )
 *            [`A of T]              ( false, [T] )
 *            [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
 *            [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )
 *           - The 2nd field is true if the tag contains a
 *             constant (empty) constructor.
 *           - '&' occurs when several types are used for the same constructor
 *             (see 4.2 in the manual)
 *           - TODO: switch to a record representation, and keep location
 *         *\)
 *   | Rinherit of core_type
 *   (\* [ T ] *\)
 *
 * and object_field =
 *   | Otag of label loc * attributes * core_type
 *   | Oinherit of core_type *)
