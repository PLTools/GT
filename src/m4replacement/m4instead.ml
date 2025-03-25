open Ppxlib

let sprintf = Printf.sprintf

type kind =
  | Show
  | Html
  | Enum
  | Fmt
  | Foldl
  | Foldr
  | Eq
  | Compare
  | Gmap
  | Eval
  | Stateful

let string_of_kind = function
  | Html -> "html"
  | Show -> "show"
  | Enum -> "enum"
  | Fmt -> "fmt"
  | Foldl -> "foldl"
  | Foldr -> "foldr"
  | Eq -> "eq"
  | Compare -> "compare"
  | Gmap -> "gmap"
  | Eval -> "eval"
  | Stateful -> "stateful"

let all_plugins =
  [ Enum; Html; Show; Fmt; Foldl; Foldr; Eq; Compare; Gmap; Eval; Stateful ]

let prepare ~loc name =
  let open Ppxlib.Ast_builder.Default in
  let pexp_id ~loc fmt =
    Format.kasprintf (fun s -> pexp_ident ~loc (Located.mk ~loc (Lident s))) fmt
  in

  let tname = ptyp_constr ~loc (Located.mk ~loc (lident name)) [] in
  let trf_typ k =
    match k with
    | Show -> [%type: [%t tname] -> string]
    | Enum -> [%type: [%t tname] -> int]
    | Html -> [%type: [%t tname] -> HTML.viewer]
    | Fmt -> [%type: Format.formatter -> [%t tname] -> unit]
    | Compare -> [%type: [%t tname] -> [%t tname] -> comparison]
    | Eq -> [%type: [%t tname] -> [%t tname] -> bool]
    | Gmap -> [%type: [%t tname] -> [%t tname]]
    | Eval -> [%type: 'env -> [%t tname] -> [%t tname]]
    | Stateful -> [%type: 'env -> [%t tname] -> 'env * [%t tname]]
    | Foldl | Foldr -> [%type: 'a -> [%t tname] -> 'a]
  in

  let cl_typ =
    ptyp_constr ~loc
      (Located.mk ~loc (Lident (sprintf "%s_t" name)))
      [ [%type: 'inh]; tname; [%type: 'syn] ]
  in
  let gcata_typ =
    [%type:
      [%t
        ptyp_class ~loc
          (Located.mk ~loc (Lident (sprintf "%s_t" name)))
          [ [%type: 'inh]; tname; [%type: 'syn] ]] ->
      'inh ->
      [%t tname] ->
      'syn]
  in
  let fix_typ =
    [%type:
      (('inh -> [%t tname] -> 'syn) -> [%t cl_typ]) ->
      'inh ->
      [%t tname] ->
      'syn]
  in
  let make_plugin_class k =
    let params =
      (match k with
      | Html | Show | Enum | Fmt | Eq | Compare -> [ ptyp_var ~loc "extra" ]
      | Foldl | Foldr -> [ ptyp_var ~loc "syn"; ptyp_var ~loc "extra" ]
      | Gmap -> [ ptyp_var ~loc "extra"; ptyp_var ~loc "syn" ]
      | Eval | Stateful ->
          [ ptyp_var ~loc "extra"; ptyp_var ~loc "syn"; ptyp_var ~loc "env" ])
      |> List.map (fun x -> (x, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
    in
    let inh_params =
      match k with
      | Html ->
          [
            ptyp_constr ~loc (Located.mk ~loc (lident "unit")) [];
            ptyp_var ~loc "extra";
            ptyp_constr ~loc
              (Located.mk ~loc (Ldot (lident "HTML", "viewer")))
              [];
          ]
      | Show ->
          [
            ptyp_constr ~loc (Located.mk ~loc (lident "unit")) [];
            ptyp_var ~loc "extra";
            ptyp_constr ~loc (Located.mk ~loc (lident "string")) [];
          ]
      | Enum ->
          [
            ptyp_constr ~loc (Located.mk ~loc (lident "unit")) [];
            ptyp_var ~loc "extra";
            ptyp_constr ~loc (Located.mk ~loc (lident "int")) [];
          ]
      | Fmt ->
          [
            [%type: Format.formatter];
            ptyp_var ~loc "extra";
            ptyp_constr ~loc (Located.mk ~loc (lident "unit")) [];
          ]
      | Foldl | Foldr ->
          [ ptyp_var ~loc "syn"; ptyp_var ~loc "extra"; ptyp_var ~loc "syn" ]
      | Eq ->
          [
            ptyp_constr ~loc (Located.mk ~loc (lident name)) [];
            ptyp_var ~loc "extra";
            ptyp_var ~loc "syn";
          ]
      | Compare -> [ tname; ptyp_var ~loc "extra"; ptyp_var ~loc "comparison" ]
      | Gmap ->
          [
            ptyp_constr ~loc (Located.mk ~loc (lident "unit")) [];
            ptyp_var ~loc "extra";
            tname;
          ]
      | Eval ->
          [
            ptyp_var ~loc "env";
            ptyp_var ~loc "extra";
            ptyp_constr ~loc (Located.mk ~loc (lident name)) [];
          ]
      | Stateful ->
          [
            ptyp_var ~loc "env";
            ptyp_var ~loc "extra";
            ptyp_tuple ~loc
              [
                ptyp_var ~loc "env";
                ptyp_constr ~loc (Located.mk ~loc (lident name)) [];
              ];
          ]
    in
    let meth =
      match k with
      | Html ->
          [%expr
            fun () x -> HTML.string ([%e pexp_id ~loc "string_of_%s" name] x)]
      | Show -> [%expr fun () -> [%e pexp_id ~loc "string_of_%s" name]]
      | Enum -> [%expr fun () _ -> 0]
      | Fmt ->
          pexp_ident ~loc
            (Located.mk ~loc
               (Ldot (Lident "Format", Printf.sprintf "pp_print_%s" name)))
      | Foldl | Foldr -> [%expr fun s _ -> s]
      | Eq -> [%expr fun inh x -> x = inh]
      | Compare -> [%expr compare_primitive]
      | Gmap | Eval -> [%expr fun _ x -> x]
      | Stateful -> [%expr fun inh x -> (inh, x)]
    in
    let cstrnt =
      match k with
      | Gmap | Eval | Stateful -> [ pcf_constraint ~loc ([%type: 'syn], tname) ]
      | _ -> []
    in

    let cname = Printf.sprintf "%s_%s_t" (string_of_kind k) name in

    let fields =
      cstrnt
      @ [
          pcf_inherit ~loc Asttypes.Fresh
            (pcl_constr ~loc
               (Located.mk ~loc (lident (name ^ "_t")))
               inh_params)
            None;
          pcf_method ~loc
            (Located.mk ~loc ("t_" ^ name), Public, Cfk_concrete (Fresh, meth));
        ]
    in
    pstr_class ~loc
      [
        class_infos ~loc ~virt:Concrete ~params ~name:(Located.mk ~loc cname)
          ~expr:
            (pcl_fun ~loc Nolabel None (ppat_any ~loc)
            @@ pcl_structure ~loc
                 (class_structure ~self:(ppat_any ~loc) ~fields));
      ]
  in
  pstr_include ~loc
    (include_infos ~loc @@ pmod_structure ~loc
    @@ List.concat
         [
           [
             pstr_type ~loc Nonrecursive
               [
                 type_declaration ~loc ~name:(Located.mk ~loc name) ~params:[]
                   ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                   ~manifest:
                     (Some (ptyp_constr ~loc (Located.mk ~loc (lident name)) []));
               ];
           ];
           [
             pstr_class ~loc
               [
                 class_infos ~loc ~virt:Virtual
                   ~params:
                     (List.map
                        (fun x ->
                          (x, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
                        [
                          ptyp_var ~loc "inh";
                          ptyp_var ~loc "extra";
                          ptyp_var ~loc "syn";
                        ])
                   ~name:(Located.mk ~loc (name ^ "_t"))
                   ~expr:
                     (pcl_structure ~loc
                        (class_structure ~self:(ppat_any ~loc)
                           ~fields:
                             [
                               pcf_method ~loc
                                 ( Located.mk ~loc ("t_" ^ name),
                                   Public,
                                   Cfk_virtual
                                     [%type: 'inh -> [%t tname] -> 'syn] );
                             ]));
               ];
           ];
           List.map make_plugin_class all_plugins;
           [
             pstr_value ~loc Nonrecursive
               [
                 value_binding ~loc
                   ~pat:
                     (ppat_var ~loc
                        (Located.mk ~loc (Printf.sprintf "gcata_%s" name)))
                   ~expr:
                     [%expr
                       fun tr inh x ->
                         [%e
                           pexp_send ~loc [%expr tr]
                             (Located.mk ~loc (Printf.sprintf "t_%s" name))]
                           inh x];
               ];
           ];
           [
             pstr_value ~loc Nonrecursive
               [
                 (let gcata_id = pexp_id ~loc "gcata_%s" name in
                  let mk_field k =
                    let wrap e =
                      match k with
                      | Show | Gmap | Enum | Html -> [%expr [%e e] ()]
                      | _ -> e
                    in
                    let clas =
                      pexp_new ~loc
                      @@ Located.mk ~loc
                           (Lident (sprintf "%s_%s_t" (string_of_kind k) name))
                    in
                    pcf_method ~loc
                      ( Located.mk ~loc (string_of_kind k),
                        Public,
                        Cfk_concrete
                          ( Fresh,
                            wrap [%expr transform_gc [%e gcata_id] [%e clas]] )
                      )
                  in
                  value_binding ~loc
                    ~pat:(ppat_var ~loc (Located.mk ~loc name))
                    ~expr:
                      [%expr
                        ({
                           gcata = [%e gcata_id];
                           fix = (fun c -> transform_gc [%e gcata_id] c);
                           plugins =
                             [%e
                               pexp_object ~loc
                               @@ class_structure ~self:(ppat_any ~loc)
                                    ~fields:(List.map mk_field all_plugins)];
                         }
                          : ( [%t gcata_typ],
                              [%t
                                ptyp_object ~loc
                                  (List.map
                                     (fun k ->
                                       otag ~loc
                                         (Located.mk ~loc (string_of_kind k))
                                         (trf_typ k))
                                     all_plugins)
                                  Closed],
                              [%t fix_typ] )
                            t)]);
               ];
           ];
         ])

let name = "generify"

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr (pstr_eval (pexp_ident (lident __)) drop ^:: nil)
    in
    [
      Extension.declare name Extension.Context.Structure_item pattern
        (fun ~loc ~path:_ name -> prepare ~loc name);
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions name
