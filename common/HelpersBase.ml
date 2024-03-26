(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

let id x = x

let not_implemented fmt =
  Printf.ksprintf (Ppxlib.Location.raise_errorf "%s are not yet implemented") fmt
;;

module Option = struct
  include Stdppx.Option

  let some x = Some x

  let is_none = function
    | None -> true
    | Some _ -> false
  ;;

  let value_map ~default ~f = function
    | None -> default
    | Some x -> f x
  ;;
end

module Char = struct
  let is_uppercase char =
    let c = Char.code char in
    Char.code 'A' <= c && c <= Char.code 'Z'
  ;;

  let is_alpha char =
    let c = Char.code char in
    (Char.code 'A' <= c && c <= Char.code 'Z')
    || (Char.code 'a' <= c && c <= Char.code 'z')
  ;;

  include Stdppx.Char
end

module List = struct
  include Stdppx.List

  let split3 xs =
    List.fold_right
      (fun (a, b, c) (ac, bc, cc) -> a :: ac, b :: bc, c :: cc)
      xs
      ([], [], [])
  ;;

  let filter_map ~f xs =
    List.fold_right
      (fun x acc ->
        match f x with
        | Some v -> v :: acc
        | None -> acc)
      xs
      []
  ;;

  let hd_exn = hd
  let tl_exn = tl
  let fold2_exn = Stdppx.List.fold_left2
  let map2_exn = Stdppx.List.map2
  let last_exn xs = List.hd @@ List.rev xs
  let pp ~f xs = Printf.sprintf "[ %s ]" (String.concat "; " @@ List.map f xs)

  let fold_left0 f = function
    | [] -> failwith "wrong argument of fold_left0"
    | h :: tl -> fold_left ~f ~init:h tl
  ;;

  let concat_map ~f xs = List.concat @@ List.map f xs

  let empty = function
    | [] -> true
    | _ -> false
  ;;

  let return x = [ x ]
  let zip_exn = combine

  let rec map3_exn xs ys zs ~f =
    match xs, ys, zs with
    | [], [], [] -> []
    | x :: xs, y :: ys, z :: zs -> f x y z :: map3_exn ~f xs ys zs
    | _ -> failwith "Bad argument: List.map3_exn"
  ;;

  module Assoc = struct
    let find_exn ~equal xs key =
      let _key, v = Stdlib.List.find (fun (k, _) -> equal k key) xs in
      v
    ;;

    let find ~equal xs key =
      try
        let _key, v = Stdlib.List.find (fun (k, _) -> equal k key) xs in
        Some v
      with
      | Not_found -> None
    ;;
  end

  let find xs ~f =
    try Some (List.find f xs) with
    | Not_found -> None
  ;;

  let remove_consecutive_duplicates ~equal =
    let rec helper = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: y :: tl when equal x y -> helper (x :: tl)
      | x :: tl -> x :: helper tl
    in
    helper
  ;;
end

open Ppxlib

let compare_core_type a b =
  String.compare
    (Format.asprintf "%a" Pprintast.core_type a)
    (Format.asprintf "%a" Pprintast.core_type b)
;;

let visit_typedecl
  ~loc
  ?(onrecord = fun _ -> not_implemented ~loc "record types")
  ?(onmanifest = fun _ -> not_implemented ~loc "manifest")
  ?(onvariant = fun _ -> not_implemented ~loc "algebraic types")
  ?(onabstract = fun _ -> not_implemented ~loc "abstract types without manifest")
  ?(onopen = fun () -> not_implemented ~loc "open types")
  tdecl
  =
  match tdecl.ptype_kind with
  | Ptype_record r -> onrecord r
  | Ptype_open -> onopen ()
  | Ptype_variant cds -> onvariant cds
  | Ptype_abstract ->
    (match tdecl.ptype_manifest with
     | None -> onabstract ()
     | Some typ -> onmanifest typ)
;;

let affect_longident ~f = function
  | Lident x -> Lident (f x)
  | Ldot _ as l -> l
  | Lapply (_, _) as l -> l
;;

let rec map_longident ~f = function
  | Lident x -> Lident (f x)
  | Ldot (l, s) -> Ldot (l, f s)
  | Lapply (l, r) -> Lapply (l, map_longident ~f r)
;;

let lident_tail = function
  | Lident _ as l -> l
  | Ldot (_, s) -> Lident s
  | Lapply (_, _) as l -> l
;;

module SS = Stdlib.Set.Make (String)

let vars_from_core_type =
  let rec helper acc typ =
    match typ.ptyp_desc with
    | Ptyp_var s -> SS.add s acc
    | Ptyp_tuple args | Ptyp_constr (_, args) -> List.fold_left args ~init:acc ~f:helper
    | Ptyp_arrow (_, l, r) -> helper (helper acc l) r
    | Ptyp_alias (t, lab) -> SS.remove lab (helper acc t)
    | Ptyp_object (_, _)
    | Ptyp_class (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_any
    | Ptyp_poly (_, _)
    | Ptyp_package _ | Ptyp_extension _ -> acc
  in
  fun root -> helper SS.empty root
;;

let%test _ =
  let loc = Location.none in
  [ "a" ] = (vars_from_core_type [%type: 'a list] |> SS.elements)
;;

let%test _ =
  let loc = Location.none in
  [] = (vars_from_core_type [%type: int list] |> SS.elements)
;;

let vars_from_tdecl tdecl =
  let ans =
    match tdecl.ptype_manifest with
    | None -> SS.empty
    | Some typ -> vars_from_core_type typ
  in
  let of_labels ls =
    List.fold_left ~init:SS.empty ls ~f:(fun acc { pld_type } ->
      SS.union acc (vars_from_core_type pld_type))
  in
  let ans2 =
    match tdecl.ptype_kind with
    | Ptype_open | Ptype_abstract -> SS.empty
    | Ptype_record ls -> of_labels ls
    | Ptype_variant cds ->
      List.fold_left cds ~init:SS.empty ~f:(fun acc -> function
        | { pcd_args = Pcstr_tuple ts } ->
          List.fold_left ~init:SS.empty ts ~f:(fun acc x ->
            SS.union acc (vars_from_core_type x))
        | { pcd_args = Pcstr_record ls } -> SS.union acc (of_labels ls))
  in
  SS.union ans2 ans
;;

let map_core_type ?(onconstr = fun _ _ -> None) ~onvar t =
  let rec helper t =
    (* Format.printf "map_core_type,helper `%a`\n%!" Pprintast.core_type t; *)
    match t.ptyp_desc with
    | Ptyp_any -> t
    | Ptyp_var name -> Option.value (onvar name) ~default:t
    | Ptyp_constr (name, args) ->
      (match onconstr name.txt args with
       | None -> { t with ptyp_desc = Ptyp_constr (name, List.map ~f:helper args) }
       | Some t -> t)
    | Ptyp_tuple args -> { t with ptyp_desc = Ptyp_tuple (List.map ~f:helper args) }
    | Ptyp_arrow (lab, from, to_) ->
      { t with ptyp_desc = Ptyp_arrow (lab, helper from, helper to_) }
    | Ptyp_variant (rows, flg, opt) ->
      let rows =
        List.map rows ~f:(fun rf ->
          match rf.prf_desc with
          | Rinherit t -> { rf with prf_desc = Rinherit (helper t) }
          | Rtag (name, flg, ps) ->
            (* Format.printf "got tag `%s`\n%!" name.txt; *)
            let params = List.map ps ~f:helper in
            { rf with prf_desc = Rtag (name, flg, params) })
      in
      { t with ptyp_desc = Ptyp_variant (rows, flg, opt) }
    | _ -> failwith "not implemented"
  in
  helper t
;;

let list_first_some ~f xs =
  List.fold_left xs ~init:None ~f:(function
    | None -> f
    | Some ans -> fun _ -> Some ans)
;;

let is_type_used_in ~tdecl lident =
  let exception Found in
  let rec helper t =
    match t.ptyp_desc with
    | Ptyp_constr ({ txt }, _) when lident = txt -> raise Found
    | Ptyp_object _ -> ()
    | Ptyp_any | Ptyp_var _ | Ptyp_class _ | Ptyp_package _ | Ptyp_extension _ -> ()
    | Ptyp_variant (rfs, _, _) ->
      List.iter rfs ~f:(function
        | { prf_desc = Rtag (_, _, xs) } -> List.iter xs ~f:helper
        | { prf_desc = Rinherit t } -> helper t)
    | Ptyp_poly (_, t) | Ptyp_alias (t, _) -> helper t
    | Ptyp_tuple args | Ptyp_constr (_, args) -> List.iter args ~f:helper
    | Ptyp_arrow (_, l, r) ->
      helper l;
      helper r
  in
  let onrecord = List.iter ~f:(fun { pld_type = t } -> helper t) in
  try
    visit_typedecl
      tdecl
      ~loc:()
      ~onopen:(fun () -> ())
      ~onabstract:(fun _ -> ())
      ~onmanifest:helper
      ~onvariant:
        (List.iter ~f:(function
          | { pcd_args = Pcstr_tuple ls } -> List.iter ~f:helper ls
          | { pcd_args = Pcstr_record ls } -> onrecord ls))
      ~onrecord;
    false
  with
  | Found -> true
;;

(* let%expect_test _ =
  let loc = Ppxlib.Location.none in
  let tdecl =
    match [%stri type nonrec heap = t].pstr_desc with
    | Pstr_type (_, [ h ]) -> h
    | _ -> assert false
  in
  Printf.printf "%b\n" (is_type_used_in ~tdecl (Lident "t"));
  Printf.printf "%b\n" (is_type_used_in ~tdecl (Lident "heap"));
  [%expect {| |}]
;; *)

let maybe_specialiaze ~what where =
  (* Format.printf "maybe specialize: %a\n%!" Pprintast.structure_item
   *   {pstr_desc=(Pstr_type (Nonrecursive, [what])); pstr_loc=what.ptype_name.loc };
   * List.iter where ~f:(Format.printf "\t%a\n%!" Pprintast.core_type);
   * print_endline "=="; *)
  let myfold ~f ~init xs =
    List.fold_left ~init xs ~f:(function
      | Some r -> fun _ -> Some r
      | None -> f)
  in
  let rec loop t =
    (* Format.printf "loop: %a\n%!" Pprintast.core_type t; *)
    match t.ptyp_desc with
    | Ptyp_constr ({ txt = Lident s }, args) when String.equal s what.ptype_name.txt ->
      (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
      Some
        (List.map2 what.ptype_params args ~f:(fun (param, _) typ ->
           match param.ptyp_desc with
           | Ptyp_var s -> s, typ
           | _ -> failwith "should not happen"))
    | Ptyp_tuple args | Ptyp_constr (_, args) -> myfold args ~init:None ~f:loop
    | Ptyp_var _ -> None
    (* | Ptyp_record (labs, _) ->
     *   myfold labs ~init:None ~f:(fun (_,e) -> loop e) *)
    | _ -> not_implemented "TODO: maybe_specialize %s" (string_of_core_type t)
  in
  list_first_some ~f:loop where
;;

(* There we iterate over type declaration [where] and check is type [what] is used inside
   If yes, it returns Some substitution of type parameters of what.
*)
let specialize_for_tdecl ~what ~where =
  let loc = where.ptype_name.loc in
  visit_typedecl
    ~loc
    where
    ~onrecord:(fun labs ->
      maybe_specialiaze ~what @@ List.map labs ~f:(fun l -> l.pld_type)
      (* not_implemented ~loc "TODO: record types" *))
      (* ~onmanifest:(fun _ -> not_implemented ~loc "TODO: manifest") *)
    ~onmanifest:(fun t -> maybe_specialiaze ~what [ t ])
    ~onvariant:(fun cstrs ->
      list_first_some cstrs ~f:(fun c ->
        match c.pcd_args with
        | Pcstr_tuple ts -> maybe_specialiaze ~what ts
        | _ -> assert false))
    ~onabstract:(fun _ -> None)
  |> function
  | None -> []
  | Some map ->
    (* Format.printf "Found somethig: %s\n------------\n%!"
     *   (List.map map ~f:(fun (s,_) -> Printf.sprintf "(\"%s\",_)" s)
     *   |> String.concat " "); *)
    map
;;

let with_constr_typ typ ~ok ~fail =
  match typ.ptyp_desc with
  | Ptyp_constr (cid, params) -> ok cid params
  | _ -> fail ()
;;

let constr_of_tuple ?(loc = Location.none) ts =
  let new_lident = Ldot (Lident "GT", Printf.sprintf "tuple%d" @@ List.length ts) in
  let open Ppxlib.Ast_builder.Default in
  ptyp_constr ~loc (Located.mk ~loc new_lident) ts
;;

let using_type ~typename root_type =
  let loc = root_type.ptype_loc in
  let open Ppxlib.Ast_builder.Default in
  (* generation type specification by type declaration *)
  ptyp_constr ~loc (Located.lident ~loc typename) (List.map ~f:fst root_type.ptype_params)
;;

let is_polyvariant typ =
  match typ.ptyp_desc with
  | Ptyp_variant (_, _, _) -> true
  | _ -> false
;;

let is_tuple typ =
  match typ.ptyp_desc with
  | Ptyp_tuple _ts -> true
  | _ -> false
;;

let is_polyvariant_tdecl tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl
    ~loc
    tdecl
    ~onopen:(fun () -> false)
    ~onrecord:(fun _ -> false)
    ~onvariant:(fun _ -> false)
    ~onabstract:(fun () -> false)
    ~onmanifest:(fun typ -> is_polyvariant typ)
;;

let is_tuple_tdecl tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl
    ~loc
    tdecl
    ~onopen:(fun () -> false)
    ~onrecord:(fun _ -> false)
    ~onvariant:(fun _ -> false)
    ~onabstract:(fun () -> false)
    ~onmanifest:(fun typ -> is_tuple typ)
;;

let is_algebraic_tdecl tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl
    ~loc
    tdecl
    ~onopen:(fun () -> false)
    ~onrecord:(fun _ -> false)
    ~onvariant:(fun _ -> true)
    ~onabstract:(fun () -> false)
    ~onmanifest:(fun _typ -> false)
;;

let has_many_constructors_tdecl tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl
    ~loc
    tdecl
    ~onopen:(fun () -> false)
    ~onrecord:(fun _ -> false)
    ~onvariant:(fun cs -> Stdppx.Int.( > ) (List.length cs) 1)
    ~onabstract:(fun () -> false)
    ~onmanifest:(fun typ ->
      match typ.ptyp_desc with
      | Ptyp_variant (rf, _, _labels) ->
        (* TODO: we don't take to account labels here *)
        List.length rf > 1
      | _ -> false)
;;

let unfold_tuple t =
  match t.ptyp_desc with
  | Ptyp_tuple ts -> ts
  | _ -> [ t ]
;;

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
  let open Ppxlib.Ast_builder.Default in
  let k cs = pexp_match ~loc what cs in
  let rs =
    List.map rows ~f:(function
      | Rtag (lab, _, args) ->
        let args =
          match args with
          | [ t ] -> unfold_tuple t
          | [] -> []
          | _ -> failwith "we don't support conjunction types"
        in
        let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
        let lhs =
          ppat_variant ~loc lab.txt
          @@
          match args with
          | [] -> None
          | _ ->
            Some
              (ppat_tuple ~loc
              @@ List.map ~f:(fun s -> ppat_var ~loc (Located.mk ~loc s)) names)
        in
        case ~guard:None ~lhs ~rhs:(onrow lab @@ List.combine names args)
      | Rinherit typ ->
        (match typ.ptyp_desc with
         | Ptyp_constr ({ txt; loc }, ts) ->
           let newname = "subj" in
           let lhs =
             ppat_alias
               ~loc
               (ppat_type ~loc (Located.mk ~loc txt))
               (Located.mk ~loc newname)
           in
           case ~guard:None ~lhs ~rhs:(oninherit ts txt newname)
         | _ -> failwith "this inherit field isn't supported"))
  in
  let ls =
    match labels with
    | None -> []
    | Some ls ->
      List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs =
          ppat_alias
            ~loc
            (ppat_type ~loc (Located.mk ~loc (Lident lab)))
            (Located.mk ~loc newname)
        in
        case ~guard:None ~lhs ~rhs:(onlabel lab newname))
  in
  k @@ rs @ ls
;;

let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t, _) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")
;;

let notify fmt =
  Format.kasprintf
    (fun s ->
      let _cmd = Printf.sprintf "notify-send \"%s\"" s in
      let (_ : int) = Stdlib.Sys.command _cmd in
      ())
    fmt
;;

let string_after_a n = int_of_char 'a' |> ( + ) n |> char_of_int |> String.make 1

external hash_variant : string -> int = "caml_gt_hash_variant"

(* TODO: Don't use and remove this function *)
let failwiths fmt = Format.kasprintf failwith fmt
