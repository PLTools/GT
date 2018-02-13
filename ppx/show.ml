(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppx_core
open Printf
open Asttypes
open Parsetree
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let g = object(self: 'self)
  inherit ['self] Plugin.generator

  method plugin_name = "show"
  method default_inh = let loc = Location.none in [%type: unit]
  method default_syn tdecl =
    let loc = tdecl.ptype_loc in
    [%type: string]

  method plugin_class_params tdecl = List.map ~f:fst tdecl.ptype_params

  method make_class ~loc tdecl ~is_rec mutal_names =
  let cur_name = tdecl.ptype_name.txt in

  let rez_names =  map_type_param_names tdecl.ptype_params ~f:id in
  let ans ?(is_poly=false) fields =
    let syn_of_param ~loc _ = self#default_syn tdecl in
    self#wrap_class_definition ~loc mutal_names tdecl ~is_poly fields
      ~inh_params:(let inh_params = prepare_param_triples ~loc
                       ~inh:(fun ~loc _ -> self#default_inh)
                       ~syn:syn_of_param
                       ~default_syn:(self#default_syn tdecl)
                       (List.map ~f:fst tdecl.ptype_params)
                   in
                   if is_poly
                   then inh_params @
                        [ Typ.constr ~loc (Located.lident ~loc (cur_name^"_open")) @@
                          List.map ("polyvar_extra"::rez_names) ~f:(Typ.var ~loc) ]
                   else inh_params
                  )
      ~default_syn:(
        (if is_poly then (cur_name^"_open", "polyvar_extra"::rez_names)
         else (cur_name,rez_names))
        |> (fun (name,param_names) ->
          Typ.constr ~loc (Located.lident ~loc name) @@
          List.map param_names ~f:(Typ.var ~loc)
        )
      )
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
  self#got_typedecl tdecl is_self_rec (fun ~is_poly -> ans ~is_poly)

  method got_constr ~loc tdecl is_self_rec do_typ cid cparams k =
    let ans args =
      [ Cf.inherit_ ~loc @@ Cl.apply
          (Cl.constr
             ({cid with txt = map_longident cid.txt
                            ~f:(sprintf "%s_%s" self#plugin_name)})
             cparams)
          (nolabelize args)
      ]
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    k @@ ans @@
    (Exp.sprintf ~loc "%s" Plugin.self_arg_name) ::
    (List.concat_map cparams ~f:(do_typ ~loc is_self_rec))

  method got_polyvar ~loc tdecl do_typ is_self_rec rows k =
    k @@
    List.map rows ~f:(function
        | Rinherit typ ->
          with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
                let args = List.concat_map params ~f:(self#do_typ ~loc is_self_rec) in
                (* gmap has blownup_params here. Maybe we should abstract this *)
                let inh_params = params @ [[%type: 'polyvar_extra]] in
                Cf.inherit_ ~loc @@ Cl.apply
                  (Cl.constr
                     ({cid with txt = map_longident cid.txt ~f:((^)"show_")})
                     inh_params
                  )
                  (nolabelize ([%expr _fself]::args))
              )
        | Rtag (constr_name,_,_,args) ->
          let names = make_new_names (List.length args) in

          Cf.method_concrete ~loc ("c_" ^ constr_name)
            [%expr fun () -> [%e
              Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
              if List.length args = 0
              then Exp.constant ~loc (Pconst_string ("`"^constr_name, None))
              else
                List.fold_left
                  (List.zip_exn names args)
                  ~f:(fun acc (name, typ) ->
                      Exp.apply_nolabeled ~loc acc
                        (self#do_typ ~loc is_self_rec ~with_arg:name typ) )
                  ~init:[%expr Format.sprintf [%e
                      let fmt = String.concat ~sep:", " @@ List.map names
                          ~f:(fun _ -> "%a")
                      in
                      Exp.constant ~loc @@  const_string @@
                      sprintf "`%s(%s)" constr_name fmt
                    ]]

            ]]

      )

  method do_typ ~loc ?with_arg is_self_rec t =
    let app_arg e =
      match with_arg with
      | Some x -> [e; Exp.ident x]
      | None -> [e]
    in
    if is_self_rec t then app_arg [%expr _fself ]
    else
      match t with
      | [%type: int]    -> app_arg [%expr fun () -> string_of_int ]
      | [%type: string] -> app_arg [%expr fun () x -> x ]
      | t -> match t.ptyp_desc with
        | Ptyp_var name ->
          app_arg Exp.(sprintf "f%s" name)
        | Ptyp_tuple params ->
          app_arg @@
          Exp.apply
            (Exp.sprintf "show_tuple%d" (List.length params))
            (List.concat_map params ~f:(self#do_typ ~loc is_self_rec) |> nolabelize)
        | Ptyp_constr (_,_) when is_self_rec t ->
          app_arg [%expr _fself ]
        | Ptyp_constr ({txt}, params) ->
          app_arg @@
          Exp.apply
            (Exp.ident_of_long @@ mknoloc @@
             map_longident ~f:((^)"show_") txt)
            (List.concat_map params ~f:((self#do_typ ~loc is_self_rec)) |> nolabelize)
        | Ptyp_variant (rows, _, maybe_labels) -> begin
            let oninherit  = fun typs cident varname ->
                    Exp.apply_nolabeled ~loc
                      Exp.(ident_of_long ~loc @@ mknoloc @@
                           map_longident cident ~f:((^)"show_"))
                      ((List.concat_map typs ~f:(self#do_typ ~loc is_self_rec)) @
                       [[%expr ()]; Exp.ident ~loc varname])
            in
            let onrow = fun lab -> function
            | [] -> Exp.constant @@ const_string ("`"^lab)
            | args ->
                let fmt = List.map args ~f:(fun _ -> "%a") in
                let fmt = sprintf "`%s (%s)" lab (String.concat ~sep:"," fmt) in
                Exp.apply_nolabeled ~loc
                  (Exp.apply1 ~loc [%expr Printf.sprintf]
                     (Exp.constant ~loc @@ const_string fmt))
                  (List.concat_map args ~f:(fun (name,t) ->
                       self#do_typ ~loc is_self_rec ~with_arg:name t))
            in
            match with_arg with
            | Some s ->
              prepare_patt_match_poly ~loc
                (Exp.sprintf ~loc "%s" s) rows maybe_labels
                ~onrow
                ~onlabel:(fun _ _ -> [%expr 1])
                ~oninherit
                :: []
            | None ->
              let k e = [%expr fun () foo -> [%e e]] :: [] in
              k @@ prepare_patt_match_poly ~loc
                (Exp.sprintf ~loc "foo") rows maybe_labels
                ~onrow
                ~onlabel:(fun _ _ -> [%expr 1])
                ~oninherit
          end
        | _ -> failwith "Finish it!"


  method on_tuple_constr tdecl is_self_rec cd ts =
    let loc = tdecl.ptype_loc in
    let names = make_new_names (List.length ts) in
    let constr_name = cd.pcd_name.txt in
    Cf.method_concrete ~loc ("c_"^cd.pcd_name.txt)
      [%expr fun () -> [%e
        Exp.fun_list ~args:(List.map names ~f:(Pat.sprintf "%s")) @@
        if List.length ts = 0
        then Exp.constant ~loc (Pconst_string (constr_name, None))
        else
          List.fold_left
            (List.zip_exn names ts)
            ~f:(fun acc (name, typ) ->
                Exp.apply_nolabeled ~loc acc
                  (self#do_typ ~loc ~with_arg:name is_self_rec typ) )
            ~init:[%expr Format.sprintf [%e
                let fmt = String.concat ~sep:", " @@ List.map names
                    ~f:(fun _ -> "%a")
                in
                Exp.constant ~loc @@  const_string @@
                sprintf "%s(%s)" constr_name fmt
              ]]
      ]]


end
