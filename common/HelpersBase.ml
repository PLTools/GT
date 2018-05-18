let id x = x
let raise_errorf ?loc fmt = Printf.ksprintf failwith fmt
let not_implemented ?loc fmt =
  Printf.ksprintf (raise_errorf ~loc "%s are not yet implemented") fmt

module List = struct
  include Base.List
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

  let concat_map ~f xs = List.concat @@ List.map f xs
  let empty = function [] -> true | _ -> false
end

module Format = struct
  include Caml.Format
  let easy_string f x =
    let (_:string) = flush_str_formatter () in
    f str_formatter x;
    flush_str_formatter ()

end

open Ppxlib

let compare_core_type a b =
  String.compare
    (Format.easy_string Pprintast.core_type a)
    (Format.easy_string Pprintast.core_type b)

let visit_typedecl ~loc
  ?(onrecord  =fun _ -> not_implemented ~loc "record types")
  ?(onmanifest=fun _ -> not_implemented ~loc "manifest")
  ?(onvariant =fun _ -> not_implemented ~loc "vairant types")
    tdecl =
  match tdecl.ptype_kind with
  | Ptype_record r -> onrecord r
  | Ptype_open     -> not_implemented ~loc "open types"
  | Ptype_variant cds -> onvariant cds
  | Ptype_abstract ->
      match tdecl.ptype_manifest with
      | None -> failwith "abstract types without manifest can't be supported"
      | Some typ -> onmanifest typ

open Longident
let affect_longident ~f = function
  | Lident x -> Lident (f x)
  | (Ldot _) as l -> l
  | (Lapply (_,_)) as l -> l

let rec map_longident ~f = function
  | Lident x -> Lident (f x)
  | Ldot (l,s) -> Ldot(l, f s)
  | Lapply (l,r) -> Lapply (l, map_longident ~f r)

let map_core_type ~onvar t =
  let rec helper t =
    match t.ptyp_desc with
    | Ptyp_any -> t
    | Ptyp_var name -> onvar name
    | Ptyp_constr (name, args) ->
      {t with ptyp_desc= Ptyp_constr (name, List.map ~f:helper args) }
    | Ptyp_tuple args ->
      {t with ptyp_desc= Ptyp_tuple (List.map ~f:helper args) }
    | Ptyp_variant (rows,flg,opt) ->
      let rows = List.map rows ~f:(function
          | Rinherit t -> Rinherit (helper t)
          | Rtag (name,attrs, flg, params) ->
            let params = List.map params ~f:helper in
            Rtag (name,attrs,flg, params)
        )
      in
      {t with ptyp_desc= Ptyp_variant (rows,flg,opt) }
    | _ -> t
  in
  helper t

let with_constr_typ typ ~ok ~fail =
  match typ.ptyp_desc with
  | Ptyp_constr (cid,params) -> ok cid params
  | _ -> fail ()

let constr_of_tuple ?(loc=Location.none) ts =
  let new_lident = Ldot (Lident "GT", Printf.sprintf "tuple%d" @@ List.length ts) in
  let open Ppxlib.Ast_builder.Default in
  ptyp_constr ~loc (Located.mk ~loc new_lident) ts

let using_type ~typename root_type =
  let loc = root_type.ptype_loc in
  let open Ppxlib.Ast_builder.Default in
  (* generation type specification by type declaration *)
  ptyp_constr ~loc (Located.lident ~loc typename) (List.map ~f:fst root_type.ptype_params)

let is_polyvariant typ =
  match typ.ptyp_desc with
  | Ptyp_variant (_,_,_) -> true
  | _ -> false

let is_polyvariant_tdecl tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _ -> false)
    ~onvariant:(fun _ -> false)
    ~onmanifest:(fun typ -> is_polyvariant typ)

let unfold_tuple t =
  match t.ptyp_desc with
  | Ptyp_tuple ts -> ts
  | _ -> [t]

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
  let open Ppxlib.Ast_builder.Default in
  let k cs = pexp_match ~loc what cs in
  let rs =
    List.map rows ~f:(function
        | Rtag (lab, _, _, args) ->
          let args = match args with
            | [t] -> unfold_tuple t
            | [] -> []
            | _ -> failwith "we don't support conjunction types"
          in
          let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
          let lhs = ppat_variant ~loc  lab @@ match args with
            | [] -> None
            | _  -> Some (ppat_tuple ~loc @@
                          List.map ~f:(fun s -> ppat_var ~loc (Located.mk ~loc s))
                            names)
          in
          case ~guard:None ~lhs
            ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;loc},ts) ->
            let newname = "subj" in
            let lhs = ppat_alias ~loc (ppat_type ~loc (Located.mk ~loc txt))
                (Located.mk ~loc newname)
            in
            case ~guard:None ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let ls = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = ppat_alias ~loc (ppat_type ~loc (Located.mk ~loc (Lident lab)) )
            (Located.mk ~loc newname)
        in
        case ~guard:None ~lhs ~rhs:(onlabel lab newname)
      )
  in
  k @@ rs@ls

let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")


let class_name_for_typ name = Printf.sprintf "%s_t" name
let trait_class_name_for_typ ~trait name =
  class_name_for_typ (if String.equal trait ""
                      then name
                      else Printf.sprintf "%s_%s" trait name)