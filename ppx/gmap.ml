(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 *   Evgeniy Moiseenko aka eucpp
 * St.Petersburg State University, JetBrains Research
 *)

open Ppx_core
<<<<<<< Updated upstream
open Printf
open Asttypes
open Ast_helper
open Location
open GtHelpers
open Ppx_core.Ast_builder.Default

let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (* Pprintast.core_type Format.std_formatter (Obj.magic typ);
  Format.pp_force_newline Format.std_formatter ();
  Format.pp_print_flush Format.std_formatter (); *)

  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Longident.Lident xxx},_) ->
    let b = String.equal xxx tdecl.ptype_name.txt in
    (* printf "xxx = %s, tdecl.ptype_name.txt = %s, %b\n%!" xxx tdecl.ptype_name.txt b; *)
    b
  | _ ->
    false
  )

let expr_of_arg ?(loc=Location.none) reprname typ root_type =
  let rec helper ?(loc=Location.none) ?(toplevel=false) =
   let maybe_apply e =
     if toplevel then [%expr [%e e] [%e Exp.ident reprname ] ]
=======
open Ppx_core.Ast_builder.Default
open Printf
open Asttypes
open GtHelpers
open Ppx_core.Ast_builder.Default

let expr_of_arg reprname typ root_type =
  let loc = root_type.ptype_loc in
  let rec helper ?(loc=Location.none) ?(toplevel=false) =
   let maybe_apply e =
     if toplevel then [%expr [%e e] [%e Exp.ident ~loc reprname ] ]
>>>>>>> Stashed changes
     else e
   in
  function
  | x when are_the_same x root_type ->
   if toplevel
<<<<<<< Updated upstream
   then [%expr GT.([%e Exp.(field ~loc (ident ~loc reprname)
                              (Located.lident ~loc "fx")) ]) () ]
   else [%expr GT.transform [%e Exp.ident root_type.ptype_name.txt] subj.GT.t#a this () ]
  | {ptyp_desc=Ptyp_var _alpha; _} -> Exp.(send ~loc [%expr subj.GT.t] _alpha)
=======
   then [%expr GT.([%e Exp.(field ~loc (ident reprname) (Located.lident ~loc "fx")) ]) () ]
   else [%expr GT.transform [%e Exp.ident root_type.ptype_name.txt] subj.GT.t#a this () ]
  | {ptyp_desc=Ptyp_var alpha; _} ->
    [%expr [%e Exp.(send [%expr subj.GT.t] alpha) ] ]
>>>>>>> Stashed changes
  | [%type: int]
  | [%type: GT.int] ->
    maybe_apply [%expr GT.lift GT.int.GT.plugins#gmap () ]
  | [%type: string]
  | [%type: GT.string] ->
    maybe_apply [%expr GT.transform GT.string (new GT.gmap_string_t) () ]
  | [%type: [%t? t] GT.list]
  | [%type: [%t? t] list] ->
    maybe_apply [%expr GT.lift (GT.list.GT.plugins#gmap [%e helper t]) () ]
  | {ptyp_desc=Ptyp_constr ({txt=Lident cname;_},
                           [typ_arg1]); }
   ->
   maybe_apply
<<<<<<< Updated upstream
     [%expr GT.transform
         [%e Exp.ident ~loc cname]
         [%e helper  typ_arg1 ]
         [%e Exp.new_ ~loc @@ Located.lident ~loc (sprintf "gmap_%s_t" cname) ]
         ()
     ]
  | _ ->
      Exp.field ~loc (Exp.ident ~loc reprname) (Located.lident ~loc "GT.fx") 
=======
     [%expr  GT.transform
               [%e Exp.ident ~loc cname]
               [%e helper typ_arg1 ]
               [%e Exp.new_ @@ Located.lident ~loc (sprintf "gmap_%s_t" cname) ]
               ()
     ]
  | _ ->
   [%expr [%e Exp.(field (ident reprname) (Located.lident ~loc "GT.fx")) ] ]
>>>>>>> Stashed changes
  in

  match typ with
  | {ptyp_desc=Ptyp_var _alpha; _} ->
<<<<<<< Updated upstream
      [%expr [%e
        Exp.(field ~loc (ident reprname)) 
          (Located.lident ~loc "GT.fx")
      ] () ]
  | _ -> helper ~toplevel:true typ
=======
    [%expr [%e Exp.(field (ident  reprname) (Located.lident ~loc "GT.fx")) ] () ]
  | _ -> helper ~loc ~toplevel:true typ

>>>>>>> Stashed changes


let name = "gmap"

let extra_params root_type =
  List.map root_type.ptype_params ~f:(fun (typ,v) ->
    match typ.ptyp_desc with
    | Ptyp_var name -> (Typ.var @@ "s" ^ name), v
    | _ -> assert false
    )


<<<<<<< Updated upstream
let inh  ?(loc=Location.none) _  = [%type: unit]
let synh ?(loc=Location.none) s = Typ.var @@ "s" ^s
=======
let inh  ?(loc=Location.none) _ = [%type: unit]
let synh ?(loc=Location.none) s = Typ.var @@ "s" ^ s
>>>>>>> Stashed changes

let synh_root root_type params =
  let loc = root_type.ptype_loc in
  Typ.constr ~loc (Located.lident ~loc root_type.ptype_name.txt) params


let core ?(loc=Location.none) = function
  | [%type: int] ->
     Cl.structure (Cstr.mk ~self:(Pat.any ())
      [ Cf.inherit_ Fresh (Cl.constr (Located.lident ~loc "GT.gmap_int_t") []) None
      ])
  | t ->
    let b = Buffer.create 40 in
    let fmt = Caml.Format.formatter_of_buffer b in
    Pprintast.core_type fmt (Caml.Obj.magic t);
    Caml.Format.pp_flush_formatter fmt;
    raise_errorf "%s\n%s" "not implemented?4 " (Buffer.contents b)


let meta_for_alias  ~root_type ~manifest : structure_item =
  assert false

let for_alias  ~root_type ~manifest : structure_item =
  assert false

let constructor root_type constr =
  let loc = root_type.ptype_loc in
  let name = constr.pcd_name in
  match constr.pcd_args with
  | Pcstr_tuple arg_types ->
    let arg_names = List.mapi (fun n _ -> sprintf "p%d" n) arg_types in
    let body =
      match List.combine arg_names arg_types with
      | [] -> Exp.construct (Located.lident ~loc name.txt) None
      | [(argn, argt)] -> Exp.construct (Located.lident ~loc name.txt) @@ Some (expr_of_arg argn argt root_type)
      | args ->
         let xs = List.map (fun (argn,argt) -> expr_of_arg argn argt root_type) args in
         Exp.construct (Located.lident ~loc name.txt) @@ Some (Exp.tuple ~loc xs)
    in
    let e =
      let pats = ("inh"::"subj"::arg_names) in
      List.fold_right pats ~init:body
        ~f:(fun name acc -> Exp.fun_ Nolabel None (Pat.var @@ mknoloc name) acc)
    in
    Cf.method_ ("c_" ^ name.txt) Public (Cfk_concrete (Fresh, e))
  | _ -> failwith "Non-tuple constructor arguments are not supported"
