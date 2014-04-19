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

EXTEND
  GLOBAL: sig_item str_item ctyp class_expr expr type_decl; 

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

  trait: [[ "["; id=LIDENT; "]" -> id ]];

  str_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> fst (generate t loc) 
  ]];

  sig_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> snd (generate t loc)
  ]];

  t_decl: [[
    "["; t=type_decl; "]" -> t, [] 
  | a=fargs; n=LIDENT; "="; t=rhs ->
      let (is_private, (def, t)), deriving = t in
      let replace = function
        | Instance (t, args, qname) as orig when qname = [n] ->
	    (try
	       let args = 
                 map (function 
		      | Variable (_, a) -> a
		      | _ -> invalid_arg "Not a variable"
		     ) 
		     args 
	       in
	       Self (t, args, qname)
	     with Invalid_argument "Not a variable" -> orig
	    )				      
	| x -> x
      in
      let t =
	match t with
	| `Tuple t -> 
	    let rec replace_tuple t = 
              function `Type t -> `Type (replace t) | `Tuple t -> `Tuple (map replace_tuple t)
	    in
	    `Tuple (replace_tuple t)
	| `Struct d -> `Struct (map (fun (i, t) -> i, replace t) d)
	| `Vari y | `Poly y -> 
	    let y =
	      map (function 
	           | `Type _ as t  -> t
	           | `Con (name, args) -> `Con (name, map replace args)
		  ) 
	          y
	    in
	    (match t with `Vari _ -> `Vari y | _ -> `Poly y)
      in
      let descriptor, typ =
        (map fst a, n, t), 
        {tdNam = VaVal (loc, VaVal n);
         tdPrm = VaVal (map (fun (name, variance) -> VaVal (Some name), variance) a);
         tdPrv = VaVal is_private;
         tdDef = def; 
         tdCon = VaVal []
        }         
      in
      typ, [descriptor, deriving]
  ]];

  rhs: [[b=rhs_base; d=OPT deriving -> b, match d with None -> [] | Some d -> d]];

  deriving: [["with"; s=LIST1 LIDENT SEP "," -> s]];

  rhs_base: [[ vari | poly | other ]];

  other: [[
    "{"; lds=label_declarations; "}" -> let d, t = split lds in false, (<:ctyp< { $list:t$ } >>, `Struct d)
  | i=instance -> false, (ctyp_of i, `Vari [`Type i])
  | t=tuple -> false, (fst t, `Tuple (snd t))
  ]];

  tuple: [[
    t=c_typ -> ctyp_of t, `Type t
  | t=LIST1 SELF SEP "*" -> let f, s = split t in <:ctyp< ( $list:f$ ) >>, `Tuple s
  | "("; t=SELF; ")" -> t
  ]];

  label_declarations: [[
    ld=label_declaration; ";"; ldl = SELF -> ld::ldl
  | ld=label_declaration; ";" -> [ld]
  | ld=label_declaration -> [ld] 
  ]];
  
  label_declaration: [[ 
    i=LIDENT; ":"; t=c_typ -> (i, t), (loc, i, false, ctyp_of t)
  | "mutable"; i=LIDENT; ":"; t=c_typ -> (i, t), (loc, i, true, ctyp_of t) 
  ]];

  vari: [[
    p=OPT "private"; OPT "|"; vari_cons=LIST1 vari_con SEP "|" -> 
      let x, y = split vari_cons in
      (p <> None), (<:ctyp< [ $list:x$ ] >>, `Vari y)
  ]];

  vari_con: [[
    c=UIDENT; a=con_args -> (loc, VaVal c, fst a, None), `Con (c, snd a) 
  ]];

  poly: [[  
    "["; body=poly_body; "]" ->
      let lcons, y = body in
      let lcons =
        map (function 
             | `Con con -> con 
             | `Type t  -> <:poly_variant< $t$ >>
            ) 
            lcons
      in
      false, 
      (<:ctyp< [ = $list:lcons$ ] >>, `Poly y)
  ]];

  poly_body: [[
    OPT "|"; poly_cons=LIST1 poly_con SEP "|" ->
      let x, y = split poly_cons in
      let lcons = map (function 
                       | `Con (loc, name, args, _) ->
                          if length args = 0 
                          then `Con <:poly_variant< `$name$ >>         
                          else                          
                            let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in
                            `Con <:poly_variant< `$name$ of $flag:false$ $list:args$ >> 
                       | `Type t -> `Type t
                      ) 
                      x
      in
      lcons, y
  ]];

  poly_con: [[
    "`"; c=UIDENT; a=con_args -> `Con  (loc, c, get_val loc (fst a), None), `Con (c, snd a) 
  | t=instance -> 
      match t with 
      | Instance (typ, a, q) -> `Type typ, `Type t 
      | _ -> oops loc "variable is not allowed here"
  ]];

  con_args: [[
    "of"; a=LIST1 c_typ SEP "*" -> VaVal (map ctyp_of a), a 
  |                             -> VaVal [], [] 
  ]];

  c_typ: [[
    t=instance       -> t
  | "["; t=ctyp; "]" -> Arbitrary t
  ]];

  instance: [
     "apply" LEFTA
     [ a=SELF; q=qname ->       
       Instance (ctyp_of_instance loc [ctyp_of a] (ctyp_of_qname loc q), [a], q)
     ] |
     "simple"   
     [   "'"; a=LIDENT -> Variable (<:ctyp< ' $a$ >>, a)
       | "("; args=LIST1 instance SEP ","; ")"; q=qname -> 
          Instance (ctyp_of_instance loc (map ctyp_of args) (ctyp_of_qname loc q), args, q)
       | q=qname -> Instance (ctyp_of_qname loc q, [], q)
     ]
  ];

  qname: [[
    n=LIDENT              -> [n]
  | m=UIDENT; "."; q=SELF -> m :: q
  ]];

  fargs: [[
    "("; a=LIST1 targ SEP ","; ")" -> a
  | a=targ                         -> [a]
  |                                -> [] 
  ]];

  targ: [[ v=variance; "'"; a=LIDENT -> a, v ]];

  variance: [[ -> None | "+" -> Some true | "-" -> Some false ]];
  
END;
