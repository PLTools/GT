#load "q_MLast.cmo";;

open Printf
open Pcaml
open MLast
open Ploc

let oops loc str = Ploc.raise loc (Failure str)
let get_val loc = function
| VaVal x -> x
| _       -> failwith "could not get VaVal _ (should not happen)"

(* type descr_t =
 *            [ `Poly of
 *               [ `Con of string * ctyp typ list | `Type of ctyp typ ] list
 *           | `Struct of (string * bool * ctyp typ) list
 *           | `Tuple of ctyp typ list
 *           | `Vari of
 *               [ `Con of string * ctyp typ list
 *                | `Tuple of ctyp typ list
 *                | `Type of ctyp typ ]
 *               list ]
 *
 * let tdecl_to_descr loc t =
 *   let name = get_val loc (snd (get_val loc t.tdNam)) in
 *   let args =
 *     List.map (fun (x, _) ->
 *      match get_val loc x with
 * 	   | Some y -> y
 * 	   | None   -> failwith "wildcard type parameters not supported"
 *     )
 *     (get_val loc t.tdPrm)
 *   in
 *   let convert =
 *     let convert_concrete typ =
 *       let rec inner = function
 *       | <:ctyp< ( $list:typs$ ) >> as typ -> Tuple (typ, List.map inner typs)
 *       | <:ctyp< ' $a$ >> as typ -> Variable (typ, a)
 *       | <:ctyp< $t$ $a$ >> as typ ->
 *           (match inner t, inner a with
 *            | _, Arbitrary _ -> Arbitrary typ
 *            | Instance (_, targs, tname), a -> Instance (typ, targs@[a], tname)
 *            | _ -> Arbitrary typ
 *           )
 *       | <:ctyp< $q$ . $t$ >> as typ ->
 *           (match inner q, inner t with
 *           | Instance (_, [], q), Instance (_, [], t) -> Instance (typ, [], q@t)
 *           | _ -> Arbitrary typ
 *           )
 *       | (<:ctyp< $uid:n$ >> | <:ctyp< $lid:n$ >>) as typ -> Instance (typ, [], [n])
 *       | t -> Arbitrary t
 *       in
 *       let rec replace = function
 *       | Tuple (t, typs) -> Tuple (t, List.map replace typs)
 *       | Instance (t, args', qname) as orig when qname = [name] ->
 *          (try
 *            let args' =
 *              List.map (function
 * 	               | Variable (_, a) -> a
 *                  | _ -> invalid_arg "Not a variable"
 *                  )
 *              args'
 *            in
 *            if args' = args then Self (t, args, qname) else orig
 *          with Invalid_argument "Not a variable" -> orig
 *          )
 *       | x -> x
 *       in
 *       replace (inner typ)
 *     in
 *     function
 *     | <:ctyp< [ $list:const$ ] >> | <:ctyp< $_$ == $priv:_$ [ $list:const$ ] >> ->
 *        let const = List.map (fun (loc, name, args, d) ->
 * 	       match d with
 * 			   | None -> `Con (get_val loc name, List.map convert_concrete (get_val loc args))
 * 			   | _    -> oops loc "unsupported constructor declaration"
 *          )
 *          const
 *     	in
 *       `Vari const
 *
 *     | <:ctyp< { $list:fields$ } >> | <:ctyp< $_$ == $priv:_$ { $list:fields$ } >> ->
 *       let fields = List.map (fun (_, name, mut, typ) -> name, mut, convert_concrete typ) fields in
 *       `Struct fields
 *
 *     | <:ctyp< ( $list:typs$ ) >> -> `Tuple (List.map convert_concrete typs)
 *
 *     | <:ctyp< [ = $list:variants$ ] >> ->
 *       let wow () = oops loc "unsupported polymorphic variant type constructor declaration" in
 *       let variants =
 *         List.map (function
 * 	       | <:poly_variant< $typ$ >> ->
 *             (match convert_concrete typ with
 *             | Arbitrary _ -> wow ()
 *             | typ -> `Type typ
 *             )
 * 	       | <:poly_variant< ` $c$ >> -> `Con (c, [])
 * 	       | <:poly_variant< ` $c$ of $list:typs$ >> ->
 *              let typs =
 *                List.flatten (
 *                  List.map (function
 *                  | <:ctyp< ( $list:typs$ ) >> -> List.map convert_concrete typs
 *                  | typ -> [convert_concrete typ]
 *    	              )
 * 		             typs
 *               )
 *              in
 *              `Con (c, typs)
 * 	       | _ -> wow ()
 * 	      )
 * 	    variants
 *       in
 *       `Poly variants
 *
 *     | typ ->
 *        (match convert_concrete typ with
 *         | Arbitrary _ -> oops loc "unsupported type"
 *         | typ         ->
 *           `Vari [
 *             match typ with
 *             | Variable (t, _) -> `Tuple [Tuple (<:ctyp< ($list:[t]$) >>, [typ])]
 *             | _ -> `Type typ
 *           ]
 * 	     )
 *   in
 *   (args, name, convert t.tdDef) *)

module Migr =
  Migrate_parsetree.Convert(Migrate_parsetree.OCaml_current)(Migrate_parsetree.OCaml_405)



let generate_str tdecls loc =
  (* Printf.printf "generate_str with length = %d\n%!" (List.length tdecls); *)

  let info = snd @@ List.hd @@ List.rev tdecls in
  (* Printf.printf "plugins: %s\n%!" (String.concat ", " info); *)
  (* let generator_f =
   *   let module H = Expander.Make(Camlp5Helpers) in
   *   H.str_type_decl ~loc ~path:""
   *     ~use_show:(if List.mem "show" info
   *                then Expander.Use [] else Expander.Skip)
   *     ~use_show_type:(if List.mem "show_typed" info
   *                     then Expander.Use [] else Expander.Skip)
   *     ~use_gmap:(if List.mem "gmap" info
   *                then Expander.Use [] else Expander.Skip)
   *     ~use_eq:(if List.mem "eq" info
   *              then Expander.Use [] else Expander.Skip)
   *     ~use_compare:(if List.mem "compare" info
   *                   then Expander.Use [] else Expander.Skip)
   *     ~use_foldl:(if List.mem "foldl" info
   *                 then Expander.Use [] else Expander.Skip)
   * in *)
  let generator_f si =
    let module H = Expander.Make(Camlp5Helpers) in
    let wrap s = s, (if List.mem s info
                     then Expander.Use [] else Expander.Skip)
    in
    H.str_type_decl_many_plugins ~loc si
      [ wrap "show"
      ; wrap "show_typed"
      ; wrap "gmap"
      ; wrap "eq"
      ; wrap "compare"
      ; wrap "foldl"
      ; wrap "html"
      ]
  in
  let out =
    let sis = <:str_item< type $list:(List.map fst tdecls)$ >>  in
    let caml_ast = Ast2pt.implem "asdf" [sis] in
    let () = assert (List.length caml_ast = 1) in
    match (List.hd caml_ast).pstr_desc with
    | Pstr_type (flg, tds) ->
       let copied = List.map Migr.copy_type_declaration tds in
       let module H = Expander.Make(Camlp5Helpers) in
       generator_f [sis] (Recursive, copied)
    |  _ -> failwith "type declaration expected"
  in

  <:str_item< declare $list:out$ end >>

let generate_sig tdecls loc =
  let info = snd @@ List.hd @@ List.rev tdecls in
  let generator_f si =
    let module H = Expander.Make(Camlp5Helpers) in
    let wrap s = s, (if List.mem s info
                     then Expander.Use [] else Expander.Skip)
    in
    H.sig_type_decl_many_plugins ~loc si
      [ wrap "show"
      ; wrap "show_typed"
      ; wrap "gmap"
      ; wrap "eq"
      ; wrap "compare"
      ; wrap "foldl"
      ; wrap "html"
      ]
  in
  (* let generator_f =
   *   let module H = Expander.Make(Camlp5Helpers) in
   *   H.sig_type_decl ~loc ~path:""
   *     ~use_show:(if List.mem "show" info
   *                then Expander.Use [] else Expander.Skip)
   *     ~use_show_type:(if List.mem "show_typed" info
   *                     then Expander.Use [] else Expander.Skip)
   *     ~use_gmap:(if List.mem "gmap" info
   *                then Expander.Use [] else Expander.Skip)
   *     ~use_eq:(if List.mem "eq" info
   *              then Expander.Use [] else Expander.Skip)
   *     ~use_compare:(if List.mem "compare" info
   *                   then Expander.Use [] else Expander.Skip)
   *     ~use_foldl:(if List.mem "foldl" info
   *                 then Expander.Use [] else Expander.Skip)
   * in *)

  let out =
   List.flatten @@
   List.map (fun (t,info) ->
     let sis = <:sig_item< type $list:[t]$ >>  in
     let caml_ast = Ast2pt.interf "asdf" [sis] in
     assert (List.length caml_ast  =  1);
     match (List.hd caml_ast).psig_desc with
     | Psig_type (flg, [td]) ->
       let copied = Migr.copy_type_declaration td in
       let module H = Expander.Make(Camlp5Helpers) in
       generator_f [sis] (Recursive, [copied])
     | _ -> assert false
     (* <:str_item< type $list:[t]$ >> *)
   ) tdecls
  in

  <:sig_item< declare $list:out$ end >>