open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let self_arg_name = "_fself"

class virtual ['self] generator = object(self: 'self)
  method virtual plugin_name : string

  method virtual default_inh : core_type

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn : type_declaration -> core_type
  (* method virtual _syn : type_declaration -> core_type *)


  method make_trans_functions: loc:location ->
    is_rec:bool -> string list -> type_declaration list -> structure_item
    = fun ~loc ~is_rec mutal_names tdecls ->
      (* we will generate mutally recrsive showers here *)
      let make_class_name typname = sprintf "%s_%s%s" self#plugin_name typname
          (match mutal_names with [] -> "" | _ -> "_stub")
      in
      Str.value ~loc Recursive @@ List.map tdecls ~f:(fun tdecl ->
        let cur_name = tdecl.ptype_name.txt in
        let others =
          List.filter mutal_names ~f:(String.(<>) cur_name)
        in
        value_binding ~loc
          ~pat:(Pat.sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt)
          ~expr:(
            let arg_transfrs = map_type_param_names tdecl.ptype_params ~f:((^)"f") in
            let fixe = [%expr GT.fix0 ] in
            Exp.fun_list ~loc
              ~args:(List.map arg_transfrs ~f:(Pat.sprintf ~loc "%s"))
              [%expr fun () t -> [%e fixe] (fun self ->
                [%e Exp.apply1 ~loc (Exp.sprintf ~loc "gcata_%s" cur_name) @@
                  Exp.apply ~loc (Exp.new_ ~loc @@ Located.lident ~loc @@
                                  make_class_name cur_name) @@
                  (nolabelize @@
                   List.map others ~f:(Exp.sprintf ~loc "%s_%s" self#plugin_name)
                   @ [[%expr self] ]
                   @ List.map arg_transfrs ~f:(Exp.sprintf ~loc "%s")
                  )
                ]
              ) () t
              ]
          )
      )

  method cur_name tdecl = tdecl.ptype_name.txt

  method wrap_class_definition ?(is_poly=false) ~loc mutal_names tdecl
      ~(default_syn: core_type) ~inh_params fields
    =
    let cur_name = self#cur_name tdecl in
    (* inherit class_t and prepare to put other members *)
    let name = sprintf "%s_%s%s" self#plugin_name cur_name
        (match mutal_names with [] -> "" | _ -> "_stub")
    in
    let params = invariantize @@ self#plugin_class_params tdecl in
    let params =
      if is_poly then params @ [[%type: 'polyvar_extra],Invariant]
      else params
    in
    Str.class_single ~loc ~params
      ~name
      ~virt:Concrete
      ~wrap:(fun body ->
        (* constructor arguments are *)
        let names =
          List.map mutal_names
            ~f:(Pat.sprintf ~loc "%s_%s" self#plugin_name) @
          [Pat.var ~loc self_arg_name] @
          map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")
        in
        Cl.fun_list names body
      )
      @@
      [
        Cf.inherit_ (Cl.constr (Located.lident ~loc ("class_"^cur_name)) inh_params)
      ] @ fields

  method virtual make_class : loc:location ->
    type_declaration -> is_rec:bool -> string list -> structure_item

  method virtual plugin_class_params: type_declaration -> core_type list

  method make_shortend_class ~loc ~(is_rec: bool) mutal_names tdecls =
    List.map tdecls ~f:(fun tdecl ->
        let mutal_names = List.filter mutal_names ~f:(String.(<>) tdecl.ptype_name.txt) in
        let class_name = sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt in
        let stub_name = class_name ^ "_stub" in
        (* maybe it should be called proto *)
        let mut_funcs = List.map ~f:(sprintf "%s_%s" self#plugin_name) mutal_names in
        let real_args = "fself" :: (List.map ~f:((^)"f") @@ make_new_names (List.length tdecl.ptype_params)) in
        let new_params = self#plugin_class_params tdecl in
        Str.single_class ~loc ~name:class_name
          ~wrap:(Cl.fun_list @@ List.map ~f:(Pat.sprintf ~loc "%s") @@ real_args)
          ~params:(invariantize new_params)
          [ Cf.inherit_ ~loc @@ Cl.apply
              (Cl.constr ~loc (Located.lident ~loc stub_name) new_params)
              (List.map ~f:(fun s -> Nolabel, Exp.sprintf ~loc "%s" s) @@ (mut_funcs@real_args))
          ]
      )

  method do_single ~loc ~is_rec tdecl =
    [ self#make_class ~loc ~is_rec tdecl []
    ; self#make_trans_functions ~loc ~is_rec [] [tdecl]
    ]

  method do_mutals ~(loc: Location.t) ~(is_rec: bool) tdecls : structure_item list =
    (* for mutal recursion we need to generate two classes and one function *)
    let mut_names = List.map tdecls ~f:(fun td -> td.ptype_name.txt) in
    List.map tdecls ~f:(fun tdecl ->
        self#make_class ~loc ~is_rec:true tdecl @@
        List.filter mut_names ~f:(String.(<>) tdecl.ptype_name.txt)
      ) @
    (self#make_trans_functions ~loc ~is_rec:true mut_names tdecls) ::
    (self#make_shortend_class  ~loc ~is_rec:true mut_names tdecls)


  method on_record_constr : type_declaration -> constructor_declaration ->
    label_declaration list -> 'on_record_result = fun _ _ _ -> failwith "not_implemented"

  method virtual on_tuple_constr : type_declaration -> (core_type -> bool) ->
    constructor_declaration ->
    core_type list -> 'on_tuple_result

  method on_variant tdecl is_self_rec cds k =
    k @@ List.map cds ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_record ls -> self#on_record_constr tdecl cd ls
        | Pcstr_tuple ts -> self#on_tuple_constr tdecl is_self_rec cd ts
      )

  (* When we got declaration of type alias via type application *)
  method virtual got_constr : loc:Location.t -> type_declaration -> (core_type -> bool) ->
    (loc:Location.t -> ?with_arg:string -> (core_type -> bool) -> core_type -> 'do_typ_res) ->
    longident Location.loc -> core_type list -> (class_field list -> 'bbb) -> 'bbb

  (* Auxilary function to construct arguements that will be passed when we instantiate
     the parent class of aliased type *)
  method virtual do_typ : loc:Location.t -> ?with_arg:string -> (core_type -> bool) ->
    core_type -> 'do_typ_res

  (* When we met polymnorphic variant on RHS of type declaration *)
  method virtual got_polyvar: loc:location -> type_declaration ->
    (loc:Location.t -> ?with_arg:string -> (core_type -> bool) -> core_type -> 'do_typ_res) ->
    (core_type -> bool) -> row_field list ->
    (class_field list -> 'pvr) -> 'pvr

  method got_typedecl tdecl is_self_rec ans =
    let loc = tdecl.ptype_loc in
    visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let rec helper typ =
          match typ.ptyp_desc with
          | Ptyp_alias (t, aname) ->
            map_core_type t ~onvar:(fun as_ ->
              if String.equal as_ aname
              then Typ.constr (Located.lident ~loc tdecl.ptype_name.txt) @@
                List.map tdecl.ptype_params ~f:fst
              else Typ.var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
            self#got_constr ~loc tdecl is_self_rec (self#do_typ)
              cid params (ans ~is_poly:false)
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
            self#got_polyvar ~loc tdecl (self#do_typ)
              is_self_rec rows
              (ans ~is_poly:true)
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds -> self#on_variant tdecl is_self_rec cds (ans ~is_poly:false))

end
