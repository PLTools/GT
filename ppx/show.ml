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

module Exp =
  struct
    include Exp

    let make_pair a b = tuple [a;b]
    let make_list xs =
      List.fold_right (fun e acc -> construct (lid "::") (Some (make_pair e acc)) ) xs
                                              (Exp.construct (lid "[]") None)
  end

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
         (fun acc (tparam,_) ->
            match tparam with
            | {ptyp_desc=Ptyp_var alpha; _} ->
                [%expr [%e acc] [%e Exp.send [%expr subj.GT.t] (mknoloc alpha) ] ]
            | _ -> assert false
         )
         [%expr GT.transform [%e Exp.ident@@lid argname]]
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
  | {ptyp_desc=Ptyp_var _alpha; _} ->
   [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ] () ]
  (* | _ when are_the_same typ root_type -> *)
  | _ -> [%expr [%e helper ~toplevel:true typ ]
         (* () [%e Exp.ident @@ lid reprname ] *)
         ]

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

let constructor root_type constr =
  let name = constr.pcd_name in
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
         (* [%expr 1] *)
         [%expr
             [%e Exp.constant (Pconst_string (name.txt ^ " (", None)) ] ^
             (String.concat ", " [%e Exp.make_list xs ] ^ ")")
         ]
    in
    let e = List.fold_right (fun name acc -> Exp.fun_ Nolabel None (Pat.var @@ mknoloc name) acc) ("inh"::"subj"::arg_names) body in
    Cf.method_ (mknoloc @@ "c_" ^ name.txt) Public (Cfk_concrete (Fresh, e))
  | _ -> failwith "Non-tuple constructor arguments are not supported"
