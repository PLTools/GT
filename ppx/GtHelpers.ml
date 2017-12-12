let id x = x

module List = struct
  include ListLabels
  let split3 xs =
    List.fold_right (fun (a,b,c) (ac,bc,cc) -> (a::ac,b::bc,c::cc))
      xs ([],[],[])
  let filter_map ~f xs =
    List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []
  let last_exn xs = List.hd @@ List.rev xs

  let pp ~f xs =
    Printf.sprintf "[ %s ]" (String.concat "; " @@ List.map f xs)

  let fold_left0 f = function
  | [] -> failwith "wrong argument of fold_left0"
  | h::tl -> fold_left ~f ~init:h tl

  let empty = function [] -> true | _ -> false
end

open Ppx_core
open Ppx_core.Ast_builder.Default
let (@@) = Caml.(@@)

let lid ?(loc=Location.none) txt = { txt; loc }
let mknoloc txt = lid txt
let pexp_pair ?(loc=Location.none) a b = pexp_tuple ~loc [a; b]

module Pat = struct
  let any ?(loc=Location.none) () = ppat_any ~loc
  let constraint_ ?(loc=Location.none) = ppat_constraint ~loc
  let construct   ?(loc=Location.none) = ppat_construct  ~loc
  let tuple ?(loc=Location.none) = ppat_tuple ~loc
  let var ?(loc=Location.none) lid = ppat_var ~loc lid
end

module Exp = struct
  let apply ?(loc=Location.none) = pexp_apply ~loc
  let case ?guard lhs rhs = case ~lhs ~rhs ~guard
  let constant ?(loc=Location.none) = pexp_constant ~loc
  let construct ?(loc=Location.none) = pexp_construct ~loc

  let field ?(loc=Location.none) =
    pexp_field ~loc

  let ident ?(loc=Location.none) s = pexp_ident ~loc @@ Located.lident ~loc s
  let ident_of_long ?(loc=Location.none) l = pexp_ident ~loc l
  let make_list ?(loc=Location.none) xs =
    List.fold_right xs
      ~f:(fun e acc ->
          construct ~loc (lid @@ lident "::")
            (Some (pexp_pair ~loc e acc)) )
      ~init:(construct ~loc (lid @@ lident "[]") None)
  let match_ ?(loc=Location.none) = pexp_match ~loc
  let new_ ?(loc=Location.none) = pexp_new ~loc
  let object_ ?(loc=Location.none) = pexp_object ~loc
  let fun_ ?(loc=Location.none) = pexp_fun ~loc
  let fun_list ?(loc=Location.none) ~args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun arg acc -> pexp_fun ~loc Nolabel None arg acc)
  let send ?(loc=Location.none) = pexp_send ~loc
end

module Cl = struct
  open Ast_helper
  include Cl

  let fun_list args e =
    if List.is_empty [] then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ Asttypes.Nolabel None arg acc)

  let apply e args =
    if List.is_empty args then e else Cl.apply e args

  let fun_ ?(loc=Location.none) lab opt patt cl_expr =
    pcl_fun ~loc lab opt patt cl_expr

  let constr ?(loc=Location.none) (lid: longident_loc) ts =
    pcl_constr ~loc lid ts
end


module Typ = struct
  open Ast_helper
  include Typ
  let ground ?(loc=Location.none) s = constr (Located.lident ~loc s) []
  let class_ ?(loc=Location.none) = ptyp_class ~loc
end

module Str = struct
  open Ast_helper
  include Str

  let single_class ?(loc=Location.none) ?(virt=Asttypes.Virtual) ?(pat=[%pat? _])
      ?(wrap= (fun x -> x)) ~name ~params body =
    Str.class_ [Ci.mk ~virt ~params (mknoloc name) @@
                wrap (Ast_helper.Cl.structure (Cstr.mk pat body))
  ]

end

module Cf = struct
  let constraint_ ?(loc=Location.none) t1 t2 =
    pcf_constraint ~loc (t1,t2)
  let inherit_ ?(loc=Location.none) flg cl_expr opt =
    pcf_inherit ~loc flg  cl_expr opt
  let method_ ?(loc=Location.none) name flg kind =
    pcf_method ~loc (mknoloc name, flg, kind)
end
module Ctf = struct
  let method_ ?(loc=Location.none) name priv_flg virt_flg kind =
    pctf_method ~loc (name, priv_flg, virt_flg, kind)
  let inherit_ ?(loc=Location.none) = pctf_inherit ~loc
end

open Parsetree
let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")

open Longident
let affect_longident ~f = function
  | Longident.Lident x -> Longident.Lident (f x)
  | (Ldot _) as l -> l
  | (Longident.Lapply (_,_)) as l -> l

let nolabelize xs = List.map ~f:(fun x -> Asttypes.Nolabel,x) xs
let invariantize types = List.map types ~f:(fun x -> x,Asttypes.Invariant)

let make_gt_a_typ ?(loc=Location.none)
    ?(inh=[%type: 'inh]) ?(itself=[%type: 'type_itself])
    ?(syn=[%type: 'syn]) ?(tpoT=[%type: 'tpoT]) () =
  (* TODO: maybe add extra string argument to concat it with type variables to get
              ('a_inh, 'a, 'a_syn, 'heck) GT.a
  *)
  [%type: ([%t inh], [%t itself], [%t syn], [%t tpoT]) GT.a]

let arr_of_param ?(loc=Location.none)
    ?(loc=Location.none) ?(inh=fun s -> Typ.var @@ "i"^s)
    ?(syn=fun s -> Typ.var @@ "s"^s) t =
  (* does from 'a the 'ia -> 'a -> 'sa *)
  match t.ptyp_desc with
  | Ptyp_var n ->
      (n, [],
        [%type: [%t inh n] -> [%t Typ.var n] -> [%t syn n]] )
  | _ ->
      failwith "arr_of_param: not all type params are supported"


let params_obj ?(loc=Location.none)
    ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) root_type =
  (* converts 'a, 'b to
     < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
   *)
  let f (t,_) = arr_of_param ~inh ~syn t in
  ptyp_object ~loc (List.map ~f root_type.ptype_params) Asttypes.Closed

(* let migrate =
 *   let open Migrate_parsetree in
 *   Versions.migrate (module OCaml_405) (module OCaml_current) *)

(* let string_of_expression e =
 *   Format.set_margin 1000;
 *   let open Migrate_parsetree in
 *   let ans = Format.asprintf "%a" Pprintast.expression (migrate.Versions.copy_expression e) in
 *   Format.set_margin 80;
 *   ans *)

(* let string_of_core_type e =
 *   Format.set_margin 1000;
 *   let open Migrate_parsetree in
 *   let ans = Format.asprintf "%a" Pprintast.core_type (migrate.Versions.copy_core_type e) in
 *   Format.set_margin 80;
 *   ans *)

let using_type ~(typename: string) root_type =
  let loc = root_type.ptype_loc in
  (* generation type specification by type declaration *)
  ptyp_constr ~loc (Located.lident ~loc typename) (List.map ~f:fst @@ root_type.ptype_params)

let for_me_patt ?(loc=Location.none) () = pvar ~loc "for_me"
let for_me_expr ?(loc=Location.none) () = pexp_ident ~loc (mknoloc (Lident "for_me"))

