(**************************************************************************
 *  Copyright (C) 2012-2013
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
  GLOBAL: sig_item str_item ctyp class_expr expr; 

  ctyp: LEVEL "ctyp2" [[
    "@"; t=ctyp LEVEL "ctyp2"-> 
      let rec inner = function
      | <:ctyp< $q$ . $t$ >> -> <:ctyp< $q$ . $inner t$ >>
      | <:ctyp< $t$ $a$ >> -> <:ctyp< $inner t$ $a$ >>
      | <:ctyp< $lid:name$ >> -> <:ctyp< $lid:closed name$ >>
      | t -> oops loc "application or qualified name expected"
      in
      inner t
  ]];

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
      let n::q = rev ci in      
      rev ((match t with None -> class_t n | Some t -> trait_t n t)::q) 
  | ci=qname -> ci 
  ]];

  trait: [[ "["; id=LIDENT; "]" -> id ]];

  str_item: LEVEL "top" [[
    "generic"; t=LIST1 t_decl SEP "and" -> fst (generate t loc) 
  ]];

  sig_item: LEVEL "top" [[
    "generic"; t=LIST1 t_decl SEP "and" -> snd (generate t loc)
  ]];

  t_decl: [[
    a=fargs; n=LIDENT; "="; t=rhs ->
      let (is_private, ((def, cons), t)), deriving = t in
      let descriptor, types =
        match t with
        | `OpenPoly (b, d) ->
           (match a with 
           | f::_ when f <> b -> 
               oops loc (sprintf "type argument \"%s\" should be listed the first in the type \"%s\" definition." b n)
           | [] -> oops loc (sprintf "type \"%s\" should atleast have type argument \"%s\"." n b)
           | _  -> (a, n, t)
           ),
           (let lcons = 
              map 
                (function
                 | `Con (constr, args) ->
                     (match args with
                     | [] -> <:poly_variant< `$constr$ >>
                     | _  ->
                        let args = 
                          map 
                            (function 
                             | Arbitrary t -> replace_t loc a n t
                             | Instance (_, targs, [name]) when name = n && 
                                 (map (function Variable (_, a) -> a) targs)  (* TODO: can be Variable | Instance *)
                                 (*targs*) = a -> <:ctyp< ' $hd a$ >>
                             | Instance (_, targs, qname) ->   
                                 let targs = map (function Variable (_, a) -> a) targs in (* TODO: can be Variable | Instance *)
                                 let qtype =
                                   match rev qname with
                                   | name::qname -> 
                                      fold_right 
                                        (fun a acc -> let t = <:ctyp< $uid:a$ >> in <:ctyp< $t$ . $acc$ >>) 
                                        qname 
                                        <:ctyp< $lid:name$ >>
                                 in
                                 fold_left 
                                   (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) 
                                   qtype 
                                   targs
                             | Variable (t, _) -> t 
                            )
                            args
                        in
                        let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in 
                        <:poly_variant< `$constr$ of $flag:false$ $list:args$ >>
                     )

                 | `Type (Instance (_, args, qname)) -> 
                     let args = map (function Variable (_, a) -> a) args in (* TODO: can be Variable | Instance *)
                     let n::rest = rev qname in
                     let h::t    = rev (closed n :: rest) in
		     let base =		     
                       fold_left 
                        (fun q t -> 
                           let t = <:ctyp< $uid:t$ >> in 
                           <:ctyp< $q$ . $t$ >>
                        ) 
                        <:ctyp< $uid:h$ >> 
                        t
                     in
		     let t = fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) base args in
		     <:poly_variant< $t$ >>
                ) 
                d 
            in
            [{
              tdNam = VaVal (loc, VaVal n);
              tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
              tdPrv = VaVal false;
              tdDef = def; 
              tdCon = VaVal cons
             };
             {
              tdNam = VaVal (loc, VaVal (closed n));
              tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
              tdPrv = VaVal false;
              tdDef = <:ctyp< [ = $list:lcons$ ] >>; 
              tdCon = VaVal []            
             }]
           ) 
        | _ -> (a, n, t), 
               [{tdNam = VaVal (loc, VaVal n);
                 tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
                 tdPrv = VaVal is_private;
                 tdDef = def; 
                 tdCon = VaVal cons
                };
                {tdNam = VaVal (loc, VaVal (closed n));
                 tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
                 tdPrv = VaVal is_private;
                 tdDef = fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) <:ctyp< $lid:n$>> a; 
                 tdCon = VaVal cons
                }
               ]
      in
      types, (descriptor, deriving)
  ]];

  rhs: [[b=rhs_base; d=OPT deriving -> b, match d with None -> [] | Some d -> d]];

  deriving: [["deriving"; s=LIST1 LIDENT SEP "," -> s]];

  rhs_base: [[ vari | poly ]];
  
  vari: [[
    p=OPT "private"; OPT "|"; vari_cons=LIST1 vari_con SEP "|" -> 
      let x, y = split vari_cons in
      (p <> None), ((<:ctyp< [ $list:x$ ] >>, []), `Vari y)
  ]];

  vari_con: [[
    c=UIDENT; a=con_args -> (loc, VaVal c, fst a, None), `Con (c, snd a) 
  ]];

  poly: [[  
    "["; ">"; body=poly_body; "]"; "as"; a=targ ->
        let lcons, y = body in
        let y = 
          map
            (function
             | `Type (Variable (_, x))     -> oops loc (sprintf "type variable ('%s) is not allowed in the type sum" x)
             | `Type (Instance (_, [], _)) -> oops loc "polymorphic type expected in the type sum"
             | `Type (Instance (_, (b::_ as args), qname) as t) ->
                 (match b with 
                  | Variable (_, b) when b = a -> `Type t
                  | _ -> oops loc (sprintf "type variable '%s should be the first parameter of all types in this type sum" a);
                 )
	     | x -> x
            )
            y
        in
        let lcons = 
          map 
            (function 
             | `Con con -> con
             | `Type t ->
                let rec replace = function
                | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $replace t1$ $t2$ >>
                | <:ctyp< $t1$ . $t2$ >> -> <:ctyp< $t1$ . $replace t2$ >>
                | <:ctyp< $lid:n$ >> -> <:ctyp< $lid:closed n$ >>
                | t -> t
                in 
                <:poly_variant< $replace t$ >>
            ) 
            lcons
        in
        false,
        ((<:ctyp< ' $a$ >>, [<:ctyp< ' $a$ >>, <:ctyp< [ > $list:lcons$ ] >>]), `OpenPoly (a, y))

    | "["; body=poly_body; "]" ->
        let lcons, y = body in
        let lcons =
          map (function 
               | `Con con -> con 
               | `Type t  -> 
                  let rec replace = function
                  | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $replace t1$ $t2$ >>
                  | <:ctyp< $t1$ . $t2$ >> -> <:ctyp< $t1$ . $replace t2$ >>
                  | <:ctyp< $lid:n$ >> -> <:ctyp< $lid:closed n$ >>
                  | t -> t
                  in 
                  <:poly_variant< $replace t$ >>
              ) 
              lcons
        in
        false, 
        ((<:ctyp< [ = $list:lcons$ ] >>, []), `ClosedPoly y)
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
    "`"; c=UIDENT; a=con_args -> `Con (loc, c, get_val (fst a), None), `Con (c, snd a) 
  | t=c_typ -> `Type (ctyp_of t), `Type t
  ]];

  con_args: [[
    "of"; a=LIST1 typ SEP "*" -> VaVal (map ctyp_of a), a
  | -> VaVal [], [] 
  ]];

  typ: [[    
   "["; t=ctyp LEVEL "apply"; "]" -> Arbitrary t
  | t=c_typ -> t
  ]];

  c_typ: [[
    a=targ -> Variable (<:ctyp< ' $a$ >>, a)
  | "("; a=LIST1 SELF SEP ","; ")"; q=qname -> 
      Instance (ctyp_of_instance loc (map ctyp_of a) (ctyp_of_qname loc q), a, q)
  | a=SELF; b=qname -> Instance (<:ctyp< $ctyp_of_qname loc b$ $ctyp_of a$ >>, [a], b)
  | q=qname -> Instance (ctyp_of_qname loc q, [], q)
  ]];

(*
  qname: [
    [ q=LIST0 qualifier; x=LIDENT ->
      let x' = <:ctyp< $lid:x$ >> in
      match q with
      | []   -> x', [x]
      | h::t -> 
         let q' =
           fold_left 
             (fun q t -> 
                let t = <:ctyp< $uid:t$ >> in 
                <:ctyp< $q$ . $t$ >>
             ) 
             <:ctyp< $uid:h$ >> 
            t
         in
         <:ctyp< $q'$ . $x'$ >>, q@[x]
    ]
  ];

  qualifier: [[ x=UIDENT; "." -> x ]];
*)

  qname: [[
    m=UIDENT; "."; q=SELF -> m::q
  | n=LIDENT -> [n]
  ]];

  fargs: [[
    a=targ -> [a]  
  |  "("; a=LIST1 targ SEP ","; ")" -> a  
  | -> [] 
  ]];

  targ: [[ "'"; a=LIDENT -> a ]];
  
END;
