open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let self_arg_name = "_fself"

class virtual ['self] generator_t = object(self: 'self)
  method virtual plugin_name : string

  method virtual default_inh : core_type

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn : type_declaration -> core_type
  method virtual syn_of_param : loc:location -> string  -> core_type

  method virtual plugin_class_params: type_declaration -> core_type list
  method virtual prepare_inherit_args_for_alias: loc:location -> type_declaration -> core_type list -> core_type list

end

class virtual ['self] generator = object(self: 'self)
  inherit ['self] generator_t

  method cur_name tdecl = tdecl.ptype_name.txt

  method wrap_class_definition ?(is_poly=false) ~loc ~inh_params mutal_names
      tdecl fields =
    let cur_name = self#cur_name tdecl in
    (* inherit class_t and prepare to put other members *)
    let params =
      let ans = self#plugin_class_params tdecl in
      let ans =
        if is_poly then ans @ [[%type: 'polyvar_extra]]
        else ans
      in
      invariantize ans
    in

    Str.class_single ~loc ~params
      ~name:(sprintf "%s_%s%s" self#plugin_name cur_name
               (match mutal_names with [] -> "" | _ -> "_stub") )
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
      [ Cf.inherit_ (Cl.constr (Located.lident ~loc ("class_"^cur_name)) inh_params)
      ] @ fields

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
              (nolabelize @@
               List.map ~f:(Exp.sprintf ~loc "%s") (mut_funcs@real_args) )
          ]
      )

  (* signature for a plugin class *)
  method make_class_sig ~loc tdecl ~is_rec (mutal_names:string list) =
    (* let is_poly = is_polyvariant_tdecl tdecl in *)

    let k fields =
      [ Sig.class_ ~loc
          ~params:(invariantize @@ self#plugin_class_params tdecl)
          ~name:(sprintf "%s_%s%s" self#plugin_name (self#cur_name tdecl)
                   (match mutal_names with [] -> "" | _ -> "_stub") )
          ~virt:Concrete
          fields
      ]
    in
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
            (* self#got_constr ~loc tdecl is_self_rec self#do_typ_gen
             *   cid params (ans ~is_poly:false) *)
            k []
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
            k []
            (* self#got_polyvar ~loc tdecl self#do_typ_gen
             *   is_self_rec rows
             *   (ans ~is_poly:true) *)
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds ->
      k []
      (* self#on_variant tdecl is_self_rec cds (ans ~is_poly:false) *)
    )

  method make_class ~loc tdecl ~is_rec mutal_names =
    let cur_name = self#cur_name tdecl in
    let ans is_poly fields =
      let inh_params =
        let inh_params = prepare_param_triples ~loc
            ~inh:(fun ~loc _ -> self#default_inh)
            ~syn:self#syn_of_param
            ~default_syn:(self#default_syn tdecl)
            tdecl.ptype_params
        in
        if is_poly
        then inh_params @ [[%type: 'polyvar_extra]]
        else
          inh_params
      in
      self#wrap_class_definition ~loc mutal_names tdecl ~is_poly fields
        ~inh_params
    in

    let is_self_rec t =
      is_rec &&
      match t.ptyp_desc with
      | Ptyp_var _ -> false
      | Ptyp_constr ({txt=Lident s}, params)
        when String.equal s cur_name &&
             List.length params = List.length tdecl.ptype_params &&
             List.for_all2_exn params tdecl.ptype_params
               ~f:(fun a (b,_) -> 0=compare_core_type a b)
        -> is_rec
      | _ -> false
    in
    self#got_typedecl tdecl is_self_rec (fun ~is_poly -> ans is_poly)

  method make_trans_functions_sig: loc:location ->
    is_rec:bool -> string list -> type_declaration list -> signature
    = fun ~loc ~is_rec mutal_names tdecls ->

      List.map tdecls ~f:(fun tdecl ->
          let type_ = using_type ~typename:tdecl.ptype_name.txt tdecl in
          let type_ = [%type: [%t self#default_inh] -> [%t type_] ->
            [%t self#default_syn tdecl]]
          in
          let type_ =
            List.fold_right
              (map_type_param_names tdecl.ptype_params ~f:id)
              ~init:type_
              ~f:(fun name -> Typ.arrow ~loc Nolabel
                     [%type: [%t self#default_inh] -> [%t Typ.var name] ->
                       [%t self#syn_of_param ~loc name]]
                 )
          in

          Sig.value ~loc
            ~name:(sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt)
            ~prim:[]
            ~type_

        )

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


  method do_single_sig ~(loc:location) ~(is_rec: bool) (tdecl: type_declaration) : signature =
    List.concat
    [ self#make_class_sig ~loc ~is_rec tdecl []
    ; self#make_trans_functions_sig ~loc ~is_rec [] [tdecl]
    ]

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
    label_declaration list -> 'on_record_result
    = fun _ _ _ ->
    failwith "not_implemented"

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
  method got_constr ~loc tdecl is_self_rec do_typ cid cparams k =
    let ans args =
      [ let params = self#prepare_inherit_args_for_alias ~loc tdecl cparams in
        Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr ~loc
             ({cid with txt = map_longident cid.txt
                            ~f:(sprintf "%s_%s" self#plugin_name)})
             params)
          (nolabelize args)
      ]
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    k @@ ans @@
    (Exp.sprintf ~loc "%s" self_arg_name) ::
    (List.map cparams ~f:(do_typ ~loc is_self_rec))

  (* Auxilary function to construct arguements that will be passed when we instantiate
     the parent class of aliased type *)
  (* method virtual do_typ : loc:Location.t -> ?with_arg:string -> (core_type -> bool) ->
   *   core_type -> 'do_typ_res *)

  (* When we met polymnorphic variant on RHS of type declaration *)
  method virtual got_polyvar: loc:location -> type_declaration ->
    (loc:Location.t -> (core_type -> bool) -> core_type -> 'do_typ_res) ->
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
            self#got_constr ~loc tdecl is_self_rec self#do_typ_gen
              cid params (ans ~is_poly:false)
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
            self#got_polyvar ~loc tdecl self#do_typ_gen
              is_self_rec rows
              (ans ~is_poly:true)
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds -> self#on_variant tdecl is_self_rec cds (ans ~is_poly:false))

  method virtual generate_for_polyvar_tag : loc:location -> string ->
    (string*core_type) list -> (core_type -> bool) -> expression ->
    (expression -> 'x) -> 'x

  (* do_type_gen will return an expression which after being applied to inherited attribute
   * and subject will return synthetized one
   *)
  method do_typ_gen ~loc is_self_rec t =
    let rec helper t =
      match t.ptyp_desc with
      | Ptyp_var s -> [%expr  [%e Exp.sprintf "f%s" s]]
      | Ptyp_tuple params ->
        [%expr fun inh subj -> [%e
          Exp.apply_nolabeled
            (Exp.sprintf "%s_tuple%d" self#plugin_name (List.length params))
            (List.map ~f:helper params @ [[%expr inh]; [%expr subj]])
        ]]
      | Ptyp_constr (_,_) when is_self_rec t -> Exp.ident ~loc self_arg_name
      | Ptyp_constr ({txt},params) ->
        (* in this place it will be easier to have all plugin in single value *)
        [%expr fun inh subj -> [%e
          Exp.apply_nolabeled
            (Exp.ident_of_long @@ mknoloc @@
             map_longident ~f:(sprintf "%s_%s" self#plugin_name) txt)
            (List.map ~f:helper params @ [[%expr inh]; [%expr subj]])
        ]]
        | Ptyp_variant (rows, _, maybe_labels) -> begin
            let oninherit typs cident varname =
              Exp.apply_nolabeled ~loc
                Exp.(ident_of_long ~loc @@ mknoloc @@
                     map_longident cident ~f:(Printf.sprintf "%s_%s" self#plugin_name))
                (List.map typs ~f:helper @ [[%expr inh]; Exp.ident ~loc varname])
            in
            let onrow lab bindings =
              self#generate_for_polyvar_tag ~loc lab bindings
                is_self_rec [%expr inh] (fun x -> x)
            in
            let k e = [%expr fun inh foo  -> [%e e]] in
            k @@ prepare_patt_match_poly ~loc
              (Exp.sprintf ~loc "foo") rows maybe_labels
              ~onrow
              ~onlabel:(fun _ _ -> [%expr 1])
              ~oninherit
          end
        | _ -> failwith "Finish it!"
    in
    helper t


end
