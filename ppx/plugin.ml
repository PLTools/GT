open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let self_arg_name = "_fself"
let construct_extra_param ~loc = [%type: 'extra]

class virtual ['self] generator_t = object(self: 'self)
  method virtual plugin_name : string

  method virtual default_inh : core_type

  (* synthethized attribute for whole type declaration *)
  method virtual default_syn : type_declaration -> core_type
  method virtual syn_of_param : loc:location -> string  -> core_type

  method virtual plugin_class_params: type_declaration -> core_type list
  method virtual prepare_inherit_args_for_alias: loc:location -> type_declaration ->
    core_type list -> core_type list

  method virtual extra_class_sig_members: type_declaration -> class_type_field list
  method virtual extra_class_str_members: type_declaration -> class_field list
end

class virtual ['self] generator initial_args = object(self: 'self)
  inherit ['self] generator_t

  (* parse arguments like { _1=<expr>; ...; _N=<expr>; ...} *)
  val reinterpreted_args =
    (* printf "inital_args length %d\n%!" (List.length initial_args); *)
    let check_name s =
      (* printf "checking arg `%s`\n%!" s; *)
      try Caml.Scanf.sscanf s "_%d" (fun n -> Some n)
      with Caml.Scanf.Scan_failure _ ->
        (* printf "can't parse it\n%!"; *) None
    in
    let ans =
      List.fold_left initial_args ~init:[]
        ~f:(fun acc (lident,expr) ->
            match lident with
            | Lident s -> Option.value_map (check_name s) ~default:acc
                            ~f:(fun n -> (n,expr) :: acc)
            | _ -> acc
          )
    in
    (* printf "Total args found for plugin : %d\n%!"  (List.length ans); *)
    ans

  method show_args =
    List.iter reinterpreted_args ~f:(fun (k,e) ->
        Format.printf "%d -> %a\n%!" k Pprintast.expression e
      )

  method make_class_arg_f_sig ~loc ~synt middle_t =
    [%type: [%t self#default_inh] -> [%t middle_t] -> [%t synt] ]

  method extra_param_stub ~loc = construct_extra_param ~loc

  method extra_class_sig_members _ = []
  method extra_class_str_members _ = []
  method cur_name tdecl = tdecl.ptype_name.txt

  method wrap_class_definition ~loc ~inh_params mutal_names
      tdecl fields =
    let cur_name = self#cur_name tdecl in
    (* inherit class_t and prepare to put other members *)

    Str.class_single ~loc
      ~params:(invariantize @@ self#plugin_class_params tdecl )
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
      let mutal_names = List.filter mutal_names
          ~f:(String.(<>) tdecl.ptype_name.txt) in
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
  method make_class_sig ~loc tdecl ~is_rec (mutal_decls: type_declaration list) =
    let is_poly = is_polyvariant_tdecl tdecl in
    let openize_poly typ =
      let loc = typ.ptyp_loc in
      if is_poly then Typ.variant ~loc [Rinherit typ] Open None
      else typ
    in
    let k fields =
      [ Sig.class_ ~loc
          ~params:(invariantize @@ self#plugin_class_params tdecl)
          ~name:(sprintf "%s_%s%s" self#plugin_name (self#cur_name tdecl)
                   (match mutal_decls with [] -> "" | _ -> "_stub") )
          ~virt:Concrete
          ~wrap:(fun init ->
              let from_mutals =
                List.map mutal_decls
                  ~f:(fun tdecl ->
                      self#make_class_arg_f_sig ~loc
                          ~synt:[%type: int]
                          (using_type ~typename:tdecl.ptype_name.txt tdecl)
                    )
              in
              let for_self = self#make_class_arg_f_sig ~loc
                  ~synt:(self#default_syn tdecl)
                  (openize_poly @@ using_type ~typename:tdecl.ptype_name.txt tdecl)
              in
              let funcs_for_args = map_type_param_names tdecl.ptype_params
                  ~f:(fun name ->
                      self#make_class_arg_f_sig ~loc
                        ~synt:(self#syn_of_param ~loc name)
                        (Typ.var ~loc name)
                    )
              in
              List.fold_right ~init (from_mutals@[for_self]@funcs_for_args)
                ~f:(Cty.arrow ~loc Nolabel)
            )
          ((self#extra_class_sig_members tdecl) @ fields)
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
            (* there for type 'a list = ('a,'a list) alist
             * we inherit plugin class for base type, for example (gmap):
             *  inherit ('a,'a2,'a list,'a2 list) gmap_alist
            **)
            k [Ctf.inherit_ ~loc @@ Cty.constr ~loc
                 (Located.mk ~loc @@ map_longident cid.txt
                    ~f:(sprintf "%s_%s" self#plugin_name))
                 (self#prepare_inherit_args_for_alias ~loc tdecl params)
              ]
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
              let rr = List.map rows ~f:(function
              | Rinherit typ ->
                  (with_constr_typ typ
                     ~ok:(fun cid params ->
                       Ctf.inherit_ ~loc @@ Cty.constr ~loc
                         (Located.map (map_longident
                                         ~f:(sprintf "%s_%s" self#plugin_name)) cid)
                         (self#prepare_inherit_args_for_alias ~loc tdecl params)
                     )
                     ~fail:(fun () -> assert false)
                  )
              | Rtag (lab,_,_, typs) ->
                  Ctf.method_ ~loc (sprintf "c_%s" lab)
                    ~virt_flg:Concrete
                    (Typ.chain_arrow ~loc
                       ([self#default_inh] @ typs @ [self#default_syn tdecl]))
              )
              in
              k @@  rr
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds ->
        k @@ List.map cds
          ~f:(fun cd ->
              match cd.pcd_args with
              | Pcstr_record _ -> assert false
              | Pcstr_tuple ts ->
                Ctf.method_ ~loc ~virt_flg:Concrete ("c_"^cd.pcd_name.txt) @@
                List.fold_right ~init:(self#default_syn tdecl)
                  (self#default_inh :: ts)
                  ~f:(Typ.arrow ~loc Nolabel)

          )
    )

  method make_class ~loc tdecl ~is_rec mutal_names =
    let cur_name = self#cur_name tdecl in
    let ans fields =
      let inh_params =
        let inh_params = prepare_param_triples ~loc
            ~inh:(fun ~loc _ -> self#default_inh)
            ~syn:self#syn_of_param
            ~default_syn:(self#default_syn tdecl)
            tdecl.ptype_params
        in
        inh_params @ [self#extra_param_stub ~loc]
      in
      self#wrap_class_definition ~loc mutal_names tdecl ~inh_params
        ((self#extra_class_str_members tdecl) @ fields)
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
    self#got_typedecl tdecl is_self_rec ans

  (* When we got declaration of type alias via type application *)
  method got_constr ~loc tdecl is_self_rec do_typ cid cparams k =
    (* printf "got a constr\n%!"; *)
    (* self#show_args; *)
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

    let tail_params =
      List.mapi cparams ~f:(fun i t ->
          (* printf "checking for arg with index (%d+1)\n%!" i; *)
          try List.Assoc.find_exn reinterpreted_args ~equal:Int.equal (i+1)
          with Not_found ->
            do_typ ~loc is_self_rec t
      )
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    k @@ ans @@
    (Exp.sprintf ~loc "%s" self_arg_name) :: tail_params

  (* When we met polymnorphic variant on RHS of type declaration *)
  method virtual got_polyvar: loc:location -> type_declaration ->
    (loc:Location.t -> (core_type -> bool) -> core_type -> 'do_typ_res) ->
    (core_type -> bool) ->
    row_field list ->
    (class_field list -> 'pvr) -> 'pvr

  method got_typedecl tdecl is_self_rec
      (k: class_field list -> structure_item) : structure_item =
    let loc = tdecl.ptype_loc in
    visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let rec helper typ : structure_item =
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
              cid params k
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc ts
          | Ptyp_variant (rows,_,_) ->
            self#got_polyvar ~loc tdecl self#do_typ_gen
              is_self_rec rows k
        | _ -> assert false
        in
        helper typ
    )
    ~onvariant:(fun cds -> self#on_variant tdecl is_self_rec cds k)

  method make_trans_function_typ tdecl =
    let loc = tdecl.ptype_loc in
    let type_ = using_type ~typename:tdecl.ptype_name.txt tdecl in
    let type_ = self#make_class_arg_f_sig ~loc ~synt:(self#default_syn tdecl) type_ in
    List.fold_right
      (map_type_param_names tdecl.ptype_params ~f:id)
      ~init:type_
      ~f:(fun name -> Typ.arrow ~loc Nolabel @@
           self#make_class_arg_f_sig ~loc ~synt:(self#syn_of_param ~loc name)
             (Typ.var name)
         )

  method make_trans_function_name tdecl =
    sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt

  method make_trans_functions_sig: loc:location ->
    is_rec:bool -> string list -> type_declaration list -> signature
    = fun ~loc ~is_rec mutal_names tdecls ->

      List.map tdecls ~f:(fun tdecl ->
          let type_ = self#make_trans_function_typ tdecl in
          Sig.value ~loc
            ~name:(self#make_trans_function_name tdecl)
            type_
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
          ~pat:(Pat.sprintf ~loc "%s" @@ self#make_trans_function_name tdecl)
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
            (Exp.ident_of_long @@
             Located.mk ~loc @@
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
