(**************************************************************************
 *  Copyright (C) 2012-2014
 *  Dmitri Boulytchev (dboulytchev@math.spbu.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open List
open Printf
open Pcaml
open MLast
open Ploc
open Plugin
open Core

let tdecl_to_descr loc t =
  let name = get_val loc (snd (get_val loc t.tdNam)) in
  let args = 
    map (fun (x, _) -> 
           match get_val loc x with 
	   | Some y -> y
	   | None   -> oops loc "wildcard type parameters not supported"
        ) 
        (get_val loc t.tdPrm) 
  in
  let convert = 
    let convert_concrete typ = 
      let rec inner = function
      | <:ctyp< ( $list:typs$ ) >> as typ -> Tuple (typ, map inner typs)
      | <:ctyp< ' $a$ >> as typ -> Variable (typ, a)
      | <:ctyp< $t$ $a$ >> as typ -> 
          (match inner t, inner a with
           | _, Arbitrary _ -> Arbitrary typ
	   | Instance (_, targs, tname), a -> Instance (typ, targs@[a], tname)
	   | _ -> Arbitrary typ
          )
      | <:ctyp< $q$ . $t$ >> as typ -> 
	  (match inner q, inner t with
	   | Instance (_, [], q), Instance (_, [], t) -> Instance (typ, [], q@t)
	   | _ -> Arbitrary typ
	  )
      | (<:ctyp< $uid:n$ >> | <:ctyp< $lid:n$ >>) as typ -> Instance (typ, [], [n])
      | t -> Arbitrary t      
      in
      let rec replace = function
      | Tuple (t, typs) -> Tuple (t, map replace typs)
      | Instance (t, args', qname) as orig when qname = [name] ->
	 (try
	   let args' = 
             map (function 
	          | Variable (_, a) -> a
		  | _ -> invalid_arg "Not a variable"
		 ) 
	         args' 
	   in
	   if args' = args then Self (t, args, qname) else orig
	  with Invalid_argument "Not a variable" -> orig
	 )				      
      | x -> x
      in
      replace (inner typ)
    in
    function
    | <:ctyp< [ $list:const$ ] >> | <:ctyp< $_$ == $priv:_$ [ $list:const$ ] >> -> 
	let const = map (fun (loc, name, args, d) -> 
	                   match d with 
			   | None -> `Con (get_val loc name, map convert_concrete (get_val loc args))
			   | _    -> oops loc "unsupported constructor declaration"
			) 
	            const 
	in
	`Vari const

    | <:ctyp< { $list:fields$ } >> | <:ctyp< $_$ == $priv:_$ { $list:fields$ } >> ->
	let fields = map (fun (_, name, mut, typ) -> name, mut, convert_concrete typ) fields in
	`Struct fields

    | <:ctyp< ( $list:typs$ ) >> -> `Tuple (map convert_concrete typs)

    | <:ctyp< [ = $list:variants$ ] >> -> 
	let wow ()   = oops loc "unsupported polymorphic variant type constructor declaration" in
	let variants = 
	  map (function
	       | <:poly_variant< $typ$ >> -> 
		  (match convert_concrete typ with 
		   | Arbitrary _ -> wow ()
		   | typ -> `Type typ
		  )
	       | <:poly_variant< ` $c$ >> -> `Con (c, [])
	       | <:poly_variant< ` $c$ of $list:typs$ >> -> 
		   let typs = 
		     flatten (
		       map (function 
			    | <:ctyp< ( $list:typs$ ) >> -> map convert_concrete typs 
			    | typ -> [convert_concrete typ]
			   ) 
		           typs
		     ) 
		   in
		   `Con (c, typs)
	       | _ -> wow ()
	      ) 
	    variants 
	in
        `Poly variants

    | typ -> 
	(match convert_concrete typ with
	 | Arbitrary _ -> oops loc "unsupported type"
	 | typ         -> `Vari [match typ with Variable (t, _) -> `Tuple [Tuple (<:ctyp< ($list:[t]$) >>, [typ])] | _ -> `Type typ]
	)
  in
  (args, name, convert t.tdDef)

EXTEND
  GLOBAL: sig_item str_item class_expr class_sig_item expr ctyp type_decl; 

  class_sig_item: [[
     "inherit"; cs = class_signature -> <:class_sig_item< inherit $cs$ >>
  ]];

  class_signature:
    [ [ "["; tl = LIST1 ctyp SEP ","; "]"; id = SELF ->
          <:class_type< $id$ [ $list:tl$ ] >>
      | "object"; cst = V (OPT class_self_type);
        csf = V (LIST0 class_sig_item); "end" ->
          <:class_type< object $_opt:cst$ $_list:csf$ end >> ]
    | [ ct1 = SELF; "."; ct2 = SELF -> <:class_type< $ct1$ . $ct2$ >>
      | ct1 = SELF; "("; ct2 = SELF; ")" -> <:class_type< $ct1$ $ct2$ >> ]
    | [ i = V LIDENT -> <:class_type< $_id: i$ >>
      | i = V UIDENT -> <:class_type< $_id: i$ >> ] 
    | [ ci = class_type_longident -> 
          match ci with
          | [x]   -> <:class_type< $id:x$ >>
          | x::xs -> fold_left (fun ct y -> 
                              let t = <:class_type< $id:y$ >> in
                              <:class_type< $ct$ . $t$ >>
                     ) <:class_type< $id:x$ >> xs
      ] ]
  ;

  class_type_longident: [[
    "@"; ci=qname; t=OPT trait -> 
      let n, q = hdtl loc (rev ci) in
      rev ((match t with None -> class_t n | Some t -> trait_t t n)::q)

  | "+"; ci=qname; t=trait -> 
      let n, q = hdtl loc (rev ci) in
      rev ((trait_proto_t t n) :: q)
  ]]
  ;

  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;

  class_expr: BEFORE "simple" [[
    "["; ct = ctyp; ","; ctcl = LIST1 ctyp SEP ","; "]"; ci = class_longident ->
      <:class_expr< [ $list:(ct :: ctcl)$ ] $list:ci$ >> 
  | "["; ct = ctyp; "]"; ci = class_longident ->
      <:class_expr< [ $ct$ ] $list:ci$ >>
  | ci = class_longident -> <:class_expr< $list:ci$ >> 
  ]];

  expr: BEFORE "simple" [
   LEFTA [ "new"; i = V class_longident "list" -> <:expr< new $_list:i$ >> ]
  ];

  ctyp: BEFORE "simple" [[
    "#"; id = V class_longident "list" -> <:ctyp< # $_list:id$ >> 
  ]];

  class_longident: [[
    "@"; ci=qname; t=OPT trait -> 
      let n, q = hdtl loc (rev ci) in
      rev ((match t with None -> class_t n | Some t -> trait_t t n)::q)

  | "+"; ci=qname; t=trait -> 
      let n, q = hdtl loc (rev ci) in
      rev ((trait_proto_t t n) :: q)

  | ci=qname -> ci 
  ]];

  qname: [[
    n=LIDENT              -> [n]
  | m=UIDENT; "."; q=SELF -> m :: q
  ]];

  trait: [[ "["; id=LIDENT; "]" -> id ]];

  str_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> fst (generate t loc) 
  ]];

  sig_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> snd (generate t loc)
  ]];

  t_decl: [[
    "["; t=type_decl; "]" -> t, []
  | t=type_decl; d=OPT deriving -> t, [tdecl_to_descr loc t, match d with None -> [] | Some d -> d]
  ]];

  deriving: [["with"; s=LIST1 LIDENT SEP "," -> s]];

END;
