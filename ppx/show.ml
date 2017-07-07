(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open Ppx_deriving
open GtHelpers


(* Used when we need to check that type we working on references himself in
  it's body *)
let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (* Pprintast.core_type Format.std_formatter (Obj.magic typ);
  Format.pp_force_newline Format.std_formatter ();
  Format.pp_print_flush Format.std_formatter (); *)

  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Lident xxx},_) ->
    let b = (xxx = tdecl.ptype_name.txt) in
    (* printf "xxx = %s, tdecl.ptype_name.txt = %s, %b\n%!" xxx tdecl.ptype_name.txt b; *)
    b
  | _ ->
    false
  )

let expr_of_arg reprname typ root_type =
  let rec helper ?(toplevel=false) =
    let maybe_apply e =
      if toplevel then [%expr [%e e] [%e Exp.ident @@ lid reprname ] ]
      else e
    in
    function
    | x when are_the_same x root_type ->
     if toplevel
     then [%expr GT.([%e Exp.(field (ident @@ lid reprname) (lid "fx")) ]) () ]
     else [%expr GT.transform [%e Exp.ident@@lid root_type.ptype_name.txt] subj.GT.t#a this () ]
    | {ptyp_desc=Ptyp_var _alpha; _} ->
        [%expr [%e Exp.(send [%expr subj.GT.t] (mknoloc _alpha)) ] ]
    | [%type: int]
    | [%type: GT.int] ->
        maybe_apply [%expr GT.lift GT.int.GT.plugins#show () ]
    | [%type: string]
    | [%type: GT.string] ->
     maybe_apply [%expr GT.transform GT.string (new GT.show_string_t) () ]
    | [%type: [%t? t] GT.list]
    | [%type: [%t? t] list] ->
     maybe_apply [%expr GT.lift (GT.list.GT.plugins#show [%e helper t]) () ]
    | {ptyp_desc=Ptyp_constr ({txt=Lident cname;_},
                             [{ptyp_desc=Ptyp_constr({txt=Lident argname;_},
                                                     _)
                              }]); _ }
     when argname = root_type.ptype_name.txt ->
       let head = List.fold_left
           ~f:(fun acc (tparam,_) ->
              match tparam with
              | {ptyp_desc=Ptyp_var alpha; _} ->
                  [%expr [%e acc] [%e Exp.send [%expr subj.GT.t] (mknoloc alpha) ] ]
              | _ -> assert false
           )
           ~init:[%expr GT.transform [%e Exp.ident@@lid argname]]
           root_type.ptype_params
       in
       maybe_apply
         [%expr  GT.transform
                 [%e Exp.ident @@ lid cname]
                 ([%e head] this)
                 [%e Exp.(new_ @@ lid @@ sprintf "show_%s_t" cname) ]
                 ()
         ]
    | {ptyp_desc=Ptyp_constr ({txt=Lident cname;_},
                             [typ_arg1]); }
     ->
     maybe_apply
       [%expr  GT.transform
                 [%e Exp.ident @@ lid cname]
                 [%e helper  typ_arg1 ]
                 [%e Exp.(new_ @@ lid @@ sprintf "show_%s_t" cname) ]
                 ()
       ]
    | _ ->
      [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ] ]
  in

  match typ with
  | {ptyp_desc=Ptyp_var alpha; _} ->
    Exp.(apply (ident @@ lid @@ "for_"^alpha) [Nolabel, ident @@ lid reprname] )
    (* [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ] () ] *)
  | _ -> helper ~toplevel:true typ

let name = "show"

let extra_params _ = []

let inh _  = [%type: unit]
let synh _ = [%type: string]

let synh_root _ _ = [%type: string]

let core = function
  | [%type: int] ->
    Cl.structure (Cstr.mk (Pat.any ()) [ Cf.inherit_ Fresh (Cl.constr (lid "GT.show_int_t") []) None ])
  | t ->
    let b = Buffer.create 40 in
    let fmt = Format.formatter_of_buffer b in
    Pprintast.core_type fmt (Obj.magic t);
    Format.pp_flush_formatter fmt;
    raise_errorf "%s\n%s" "not implemented?4 " (Buffer.contents b)

let is_ground_enough = function
  | [%type: string] -> true
  | [%type: char] -> true
  | _ -> false

let process_ground_enough ~n (topTr, specTr, appTr, types, inhTypes) t =
  let wrap orig ident =
    let transformer_pat = Pat.var @@ mknoloc @@ sprintf "for_%d" n in
    let for_expr = [%expr let open GT in lift
                      [%e Exp.field ident (lid "plugins")  ] #show () ] in
    ( topTr
    , (fun e -> Cl.let_ Nonrecursive [Vb.mk transformer_pat for_expr] (specTr e))
    , (Exp.ident @@ lid @@ sprintf "for_%d" n) :: appTr
    , types
    , orig::orig::inhTypes)
  in
  match t with
  | [%type: string] -> wrap t (Exp.ident @@ lid "string")
  | [%type: char]   -> wrap t (Exp.ident @@ lid "char")
  | _ -> failwith "not ground enough"

let meta_for_alias ~name ~root_type ~manifest : structure_item =
  (* params of meta class depend only on type parameters of a type being processed *)

  let eval_params ps =
    let rec helper ~n ~acc:(topTr, specTr, appTr, types, inhTypes) = function
    | [] -> (topTr, specTr, appTr, types, inhTypes)
    | {ptyp_desc=Ptyp_var name; _} :: tl ->
        let new_params = [Typ.var name; Typ.var @@ name^"_holder"] in
        let transformer_pat = Pat.var @@ mknoloc @@ sprintf "for_%d_%s" n name in
        let transformer_expr = Exp.ident @@ lid @@ sprintf "for_%d_%s" n name in
        helper ~n:(n-1) tl
                  ~acc: ( (fun e -> Cl.fun_ Nolabel None transformer_pat (topTr e))
                        , specTr
                        , transformer_expr :: appTr
                        , new_params::types
                        , new_params@inhTypes)

    | orig :: tl  when are_the_same orig root_type ->
        (* let transformer_pat  = Pat.var @@ mknoloc @@ sprintf "for_%d_%s" n name in *)
        let transformer_expr = Exp.ident @@ lid @@ sprintf "for_%d_%s" n name in
        helper  ~n:(n-1) tl
                ~acc: ( topTr
                      , specTr
                      , transformer_expr :: appTr
                      , types
                      , inhTypes)
    | orig :: tl when is_ground_enough orig ->
        helper ~n:(n-1) tl ~acc:(process_ground_enough ~n (topTr, specTr, appTr, types, inhTypes) orig)
    (* | ([%type: string] as orig) :: tl ->
        let transformer_pat = Pat.var @@ mknoloc @@ sprintf "for_%d" n in
        let for_expr = [%expr GT.lift (GT.string.GT.plugins)#show () ] in
        helper ~n:(n-1) tl
                  ~acc: ( topTr
                        , (fun e -> Cl.let_ Nonrecursive [Vb.mk transformer_pat for_expr] (specTr e))
                        , (Exp.ident @@ lid @@ sprintf "for_%d" n) :: appTr
                        , types
                        , orig::orig::inhTypes) *)
    | typ :: _ -> failwith (sprintf "Don't know what to do about the type '%s'" (string_of_core_type typ))
    in
    let topTr, specTr, appTr, types, inhTypes =
      (* we reverse inout to make a result in th eright order *)
      helper ~n:(List.length ps) ~acc:((fun x -> x),(fun x -> x),[],[],[]) (List.rev ps) in
    let types = [%type: 'tpoT] :: (List.concat types) in
    let inhTypes = [ [%type: 'tpoT] ] @ inhTypes in
    let appTr = nolabelize appTr in
    (topTr, specTr, appTr, types, inhTypes)
  in
  match manifest.ptyp_desc with
  | Ptyp_constr ({txt=ident;_}, params) ->
    let topTr, specTr, appTr, types, inhTypes = eval_params params in
    (* let clas =
      Cl.structure (Cstr.mk (Pat.any ()) )
    in
    let clas = specTr clas in *)
    let parent_meta = affect_longident ident ~f:(sprintf "show_meta_%s") in
    Str.single_class ~params:(invariantize types) ~name
      ~wrap:(fun x -> topTr @@ specTr x)
      [
        Cf.inherit_ Fresh (Cl.apply (Cl.constr (mknoloc parent_meta) inhTypes) appTr) None
      ]
  | _ -> failwith "not implemented"

let for_alias ~name ~root_type ~manifest : structure_item =
  match manifest.ptyp_desc with
  | Ptyp_constr ({txt=ident;_}, params) ->
      let appTr = map_type_param_names root_type.ptype_params ~f:(fun _ -> [%expr (fun z -> z.GT.fx ())])
        |> nolabelize
      in
      let inh_params =
        map_type_param_names root_type.ptype_params
          ~f:(fun name ->
                let itself = Typ.var name in
                [ itself
                ; make_gt_a_typ ~inh:[%type: unit] ~itself ~syn:[%type: string] ()
                ])
        |> List.concat
        |> (fun xs -> (Typ.alias (params_obj root_type) "tpoT") :: xs  )
      in
      let typname = root_type.ptype_name.txt in
      Str.single_class ~params:root_type.ptype_params ~name ~pat:[%pat? self] ~virt:Concrete
        [
          Cf.inherit_ Fresh (Cl.apply (Cl.constr (lid @@ sprintf "show_meta_%s" typname) inh_params) appTr) None
        ; let e =
            let patts,exprs =
              map_type_param_names root_type.ptype_params
                ~f:(fun name ->
                    let name = sprintf "transform_%s" name in
                    (Pat.var @@ mknoloc name , Exp.ident @@ lid name))
              |> List.split
            in
            let exprs = nolabelize @@ ( exprs @ [[%expr self]]) in
            Exp.fun_list ~args:patts @@
              Exp.apply (Exp.ident @@ lid @@  sprintf "%s_gcata" typname) exprs
          in
          Cf.method_ (mknoloc @@ sprintf "t_%s" typname) Public (Cfk_concrete (Fresh, e) )
        ]
  | _ -> assert false

let constructor root_type constr =
  let name = constr.pcd_name in
  (* let param_names = map_type_param_names ~f:(fun x -> x) root_type.ptype_params in *)
  match constr.pcd_args with
  | Pcstr_tuple arg_types ->
    let arg_names = List.mapi (fun n _ -> sprintf "p%d" n) arg_types in
    let body =
      match List.combine arg_names arg_types with
      | [] -> Exp.constant (Pconst_string (name.txt ^ " ()", None))
      | [(argn,argt)] -> [%expr
                            [%e Exp.constant (Pconst_string (name.txt ^ " (", None)) ] ^
                            [%e expr_of_arg argn argt root_type] ^ ")"
                         ]
      | args ->
         let xs = List.map (fun (argn,argt) -> expr_of_arg argn argt root_type) args in
         [%expr
             [%e Exp.constant (Pconst_string (name.txt ^ " (", None)) ] ^
             (String.concat ", " [%e Exp.make_list xs ] ^ ")")
         ]
    in
    let e =
      let pats =
        [ Pat.var @@ mknoloc "inh"; Pat.var @@ mknoloc "subj" ] @
        (List.map2 arg_names arg_types ~f:(fun name t ->
          let always = Pat.(var @@ mknoloc name) in
          match t.ptyp_desc with
          | Ptyp_var name -> Pat.(constraint_ always (Typ.var @@ name^"_holder") )
          | _ -> always
        ) )
      in
      List.fold_right pats ~f:(fun pat acc -> Exp.fun_ Nolabel None pat acc) ~init:body
    in
    Cf.method_ (mknoloc @@ "c_" ^ name.txt) Public (Cfk_concrete (Fresh, e))
  | _ -> failwith "Non-tuple constructor arguments are not supported"
