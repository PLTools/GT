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
end

module Exp = struct
  open Ast_helper
  include Exp

  let make_pair a b = tuple [a;b]
  let make_list =
    List.fold_right ~f:(fun e acc -> construct (lid "::") (Some (make_pair e acc)) )
                    ~init:(Exp.construct (lid "[]") None)

  let fun_list ~args e =
    if args = [] then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Exp.fun_ Nolabel None arg acc)
end

module Cl = struct
  open Ast_helper
  include Cl

  let fun_list args e =
    if args = [] then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ Asttypes.Nolabel None arg acc)

  let apply e args =
    if args = [] then e else Cl.apply e args
end

module Typ = struct
  open Ast_helper
  include Typ
  let ground s = constr (lid s) []
end

module Str = struct
  open Ast_helper
  include Str

  let single_class ?(virt=Asttypes.Virtual) ?(pat=[%pat? _]) ?(wrap= (fun x -> x)) ~name ~params body =
    Str.class_ [Ci.mk ~virt ~params (Location.mknoloc name) @@
                wrap (Ast_helper.Cl.structure (Cstr.mk pat body))
  ]

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

let make_gt_a_typ ?(inh=[%type: 'inh]) ?(itself=[%type: 'type_itself]) ?(syn=[%type: 'syn]) ?(tpoT=[%type: 'tpoT]) () =
  (* TODO: maybe add extra string argument to concat it with type variables to get
              ('a_inh, 'a, 'a_syn, 'heck) GT.a
  *)
  [%type: ([%t inh], [%t itself], [%t syn], [%t tpoT]) GT.a]

let arr_of_param ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) t =
  (* does from 'a the 'ia -> 'a -> 'sa *)
  match t.ptyp_desc with
  | Ptyp_var n ->
      (Location.mknoloc n, [],
        [%type: [%t inh n] -> [%t Typ.var n] -> [%t syn n]] )
  | _ ->
      Ppx_deriving.raise_errorf "arr_of_param: not all type params are supported"


let params_obj ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) root_type =
  (* converts 'a, 'b to
     < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
   *)
  let f (t,_) = arr_of_param ~inh ~syn t in
  Typ.object_ (List.map f root_type.ptype_params) Asttypes.Closed

let migrate =
  let open Migrate_parsetree in
  Versions.migrate (module OCaml_405) (module OCaml_current)

let string_of_expression e =
  Format.set_margin 1000;
  let open Migrate_parsetree in
  let ans = Format.asprintf "%a" Pprintast.expression (migrate.Versions.copy_expression e) in
  Format.set_margin 80;
  ans

let string_of_core_type e =
  Format.set_margin 1000;
  let open Migrate_parsetree in
  let ans = Format.asprintf "%a" Pprintast.core_type (migrate.Versions.copy_core_type e) in
  Format.set_margin 80;
  ans

let using_type ~typename root_type =
  (* generation type specification by type declaration *)
  Typ.constr (lid typename) (List.map ~f:fst @@ root_type.ptype_params)

let for_me_patt = Ast_helper.Pat.var @@ Location.mknoloc "for_me"
let for_me_expr = Exp.ident @@ lid "for_me"
