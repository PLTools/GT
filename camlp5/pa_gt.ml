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
open Dynlink

let cata    name      = name ^ "_gcata"
let others            = "others"
let cmethod c         = "m_" ^ c
let apply             = "apply"
let closed  name      = name ^ "'"
let class_t name      = name ^ "_t"
let trait_t typ trait = class_t (if trait <> "" then sprintf "%s_%s" typ trait else typ)

module Plugin =
  struct

    exception Bad_plugin of string

    let load_path = ref []

    let _ =
      Pcaml.add_option "-L"
        (Arg.String (fun dir -> load_path := !load_path @ [dir]))
        "<dir> Add <dir> to the list of include directories."

    type properties = {
      inh     : ctyp;
      syn     : ctyp;
      arg_img : string -> ctyp;
    }

    type type_descriptor = {
      is_polyvar : bool;
      is_open    : [`Yes of string | `No];
      type_args  : string list;
      name       : string;
      default    : properties;
    }

    type constructor = {
      constr : string;
      acc    : string;
      subj   : string;
      args   : (string * [`Specific of string list * string list | `Variable of string | `Generic of ctyp]) list;
    }

    type t = loc -> type_descriptor -> properties * (constructor -> expr)

    module Helper (L : sig val loc : loc end) =
      struct
        
        open L

        let id lid uid s = 
          if String.length s = 0 
          then invalid_arg "MLComb.id: empty string"
          else (if s.[0] = Char.uppercase s.[0] then uid else lid) s

        let qname acc id = function
        | []    -> invalid_arg "MLComb.qname: empty string list"
        | h::tl -> fold_left (fun q n -> acc q (id n)) (id h) tl

        module T =
          struct

            let lid         = (fun s -> <:ctyp< $lid:s$ >>)
            let uid         = (fun s -> <:ctyp< $uid:s$ >>)
            let id          = id lid uid
            let acc         = qname (fun x y -> <:ctyp< $x$ . $y$ >>) (fun x -> x)
            let qname       = qname (fun x y -> <:ctyp< $x$ . $y$ >>) id 
            let alias t1 t2 = <:ctyp< $t1$ as $t2$ >>
            let wildcard    = <:ctyp< _ >>

            let app = function
            | []    -> invalid_arg "MLComb.T.app: empty expression list"
            | h::tl -> fold_left (fun e a -> <:ctyp< $e$ $a$ >>) h tl

            let arrow = function
            | []    -> invalid_arg "MLComb.T.arrow: empty expression list"
            | h::tl -> fold_left (fun e a -> <:ctyp< $e$ -> $a$ >>) h tl    

            let class_t   qname      = <:ctyp< # $list:qname$ >>
            let label     s t        = <:ctyp< ~$s$: $t$ >>
            let manifest  t1 priv t2 = <:ctyp< $t1$ == $priv:priv$ $t2$ >>
            let obj       lst ell    = <:ctyp< < $list:lst$ $flag:ell$ > >>
            let opt_label s t        = <:ctyp< ?$s$: $t$ >>
            let package   mt         = <:ctyp< (module $mt$) >>
            let polymorph lst t      = <:ctyp< ! $list:lst$ . $t$ >>
            let var       s          = <:ctyp< '$s$ >>
            let record    lst        = <:ctyp< { $list:lst$ } >>
            let sum       lst        = <:ctyp< [ $list:lst$ ] >>
            let tuple     lst        = <:ctyp< ( $list:lst$ ) >>

            let pv_constr s = function
            | []   -> <:poly_variant< ` $s$ >>
            | args -> <:poly_variant< ` $s$ of $list:args$ >>

            let pv_and_constr       s args = <:poly_variant< ` $s$ of & $list:args$ >>
            let pv_type             t      = <:poly_variant< $t$ >>
            let eq_variant          lpv    = <:ctyp< [ = $list:lpv$ ] >>
            let more_variant        lpv    = <:ctyp< [ > $list:lpv$ ] >>
            let less_variant        lpv    = <:ctyp< [ < $list:lpv$ ] >>
            let less_constr_variant lpv ls = <:ctyp< [ < $list:lpv$ > $list:ls$ ] >>

          end
 
        module P =
          struct

            let lid         = (fun s -> <:patt< $lid:s$ >>)
            let uid         = (fun s -> <:patt< $uid:s$ >>)
            let id          = id lid uid
            let acc         = qname (fun x y -> <:patt< $x$ . $y$ >>) (fun x -> x)
            let qname       = qname (fun x y -> <:patt< $x$ . $y$ >>) id 
            let alias t1 t2 = <:patt< ( $t1$ as $t2$ ) >>
            let wildcard    = <:patt< _ >>

            let app = function
            | []    -> invalid_arg "MLComb.P.app: empty expression list"
            | h::tl -> fold_left (fun e a -> <:patt< $e$ $a$ >>) h tl

            let array      lp    = <:patt< [| $list:lp$ |] >>
	    let char       s     = <:patt< $chr:s$ >>
	    let float      s     = <:patt< $flo:s$ >>
	    let int        s     = <:patt< $int:s$ >>
	    let int32      s     = <:patt< $int32:s$ >>
	    let int64      s     = <:patt< $int64:s$ >>
	    let nativeint  s     = <:patt< $nativeint:s$ >>
	    let label      p1 p2 = <:patt< ~{$p1$ $opt:p2$} >>
	    let lazy_p     p     = <:patt< lazy $p$ >>
            let newtype    s     = <:patt< (type $lid:s$) >>
	    let opt_label  p oe  = <:patt< ?{$p$ $opt:oe$} >>
	    let or_p       p1 p2 = <:patt< $p1$ | $p2$ >>
            let record     lpp   = <:patt< { $list:lpp$ } >>
	    let range      p1 p2 = <:patt< $p1$ .. $p2$ >>
	    let str        s     = <:patt< $str:s$ >>
	    let tuple      lp    = <:patt< ($list:lp$) >>
            let constr     p t   = <:patt< ($p$ : $t$) >>
	    let type_p     ls    = <:patt< # $list:ls$ >>
	    let module_unp s     = function None -> <:patt< (module $uid:s$) >> | Some mt -> <:patt< (module $uid:s$ : $mt$) >>
	    let variant    s     = <:patt< ` $s$ >>

          end

        module E = 
          struct
    
            let lid   = (fun s -> <:expr< $lid:s$ >>)
            let uid   = (fun s -> <:expr< $uid:s$ >>)
            let id    = id lid uid
            let acc   = qname (fun x y -> <:expr< $x$ . $y$ >>) (fun x -> x) 
            let qname = qname (fun x y -> <:expr< $x$ . $y$ >>) id 

            let app   = function
            | []    -> invalid_arg "MLComb.E.app: empty expression list"
            | h::tl -> fold_left (fun e a -> <:expr< $e$ $a$ >>) h tl
     
            let abstr       list       = <:expr< fun [ $list:list$ ] >>
            let aelem       a i        = <:expr< $a$ . ( $i$ ) >>
            let belem       a i        = <:expr< $a$ . { $i$ } >>
            let array       list       = <:expr< [| $list:list$ |] >>
            let assrt       e          = <:expr< assert $e$ >>
            let assign      x y        = <:expr< $x$ := $y$ >>
            let char        s          = <:expr< $chr:s$ >>
            let coerce      e t        = <:expr< ( $e$ :> $t$ ) >>
            let float       s          = <:expr< $flo:s$ >>
            let for_to      i l u list = <:expr< for $lid:i$ = $l$ to     $u$ do { $list:list$ } >>
            let for_downto  i l u list = <:expr< for $lid:i$ = $l$ downto $u$ do { $list:list$ } >>
            let if_then     c t e      = <:expr< if $c$ then $t$ else $e$ >>
            let int         s          = <:expr< $int:s$ >>
            let int32       s          = <:expr< $int32:s$ >>
            let int64       s          = <:expr< $int64:s$ >>
            let nat         s          = <:expr< $nativeint:s$ >>
            let label       p e        = <:expr< ~{$p$ $opt:e$} >>
            let lazy_e      e          = <:expr< lazy $e$ >>
            let letrec      pe e       = <:expr< let rec $list:pe$ in $e$ >>
            let let_nrec    pe e       = <:expr< let $list:pe$ in $e$ >>
            let let_module  s me e     = <:expr< let module $uid:s$ = $me$ in $e$ >>
            let match_e     e pe       = <:expr< match $e$ with [ $list:pe$ ] >>
            let new_e       list       = <:expr< new $list:list$ >>
            let object_e    p list     = <:expr< object $opt:p$ $list:list$ end >>
            let opt_label   p oe       = <:expr< ?{ $p$ $opt:oe$ } >>
            let override    list       = <:expr< {< $list:list$ >} >>
            let module_e    me         = <:expr< ( module $me$ ) >>
            let module_t    me t       = <:expr< ( module $me$ : $t$ ) >>
            let record      list       = <:expr< { $list:list$ } >>
            let record_with e list     = <:expr< { ($e$) with $list:list$} >>
            let seq         list       = <:expr< do { $list:list$ } >>
            let method_call e m        = <:expr< $e$ # $m$ >>
            let selem       s i        = <:expr< $s$ . [$i$] >>
            let str         s          = <:expr< $str:s$ >>
            let try_e       e list     = <:expr< try $e$ with [ $list:list$ ] >>
            let tuple       list       = <:expr< ( $list:list$ ) >>
            let constr      e t        = <:expr< ( $e$ : $t$ ) >>
            let variant     s          = <:expr< ` $s$ >>
            let while_e     e list     = <:expr< while $e$ do { $list:list$ } >>
            let unit                   = <:expr< () >>

            let gt_field f e = acc [e; uid "GT"; lid f]
            let f            = gt_field "f" 
            let x            = gt_field "x"
            let fx           = gt_field "fx"
            let tp e p       = method_call (gt_field "t" e) p

          end
      end
  
    let generate_classes loc trait descr (prop, _) (b_def, b_decl) =
      let class_targs =
        (match descr.is_open with `Yes s -> [s] | `No -> []) @
        flatten (map (fun a -> match prop.arg_img a with <:ctyp< ' $fa$ >> -> [a; fa]| _ -> [a]) descr.type_args) @
        (match prop.inh with <:ctyp< ' $inh$ >> -> [inh] | _ -> []) @
        (match prop.syn with <:ctyp< ' $syn$ >> -> [syn] | _ -> [])
      in 
      let def b = { 
        ciLoc = loc;
        ciVir = Ploc.VaVal false;
        ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
        ciNam = Ploc.VaVal (trait_t descr.name trait);
        ciExp = b
      } 
      in
      <:str_item< class $list:[def b_def]$ >>,
      <:sig_item< class $list:[def b_decl]$ >>

    let generate_inherit loc qname descr (prop, _) =
      let args =
        (match descr.is_open with `Yes s -> [<:ctyp< ' $s$ >>] | _ -> []) @
        flatten (map (fun a -> [<:ctyp< ' $a$ >>; prop.arg_img a]) descr.type_args) @
        [prop.inh; prop.syn]
      in
      let ce    = <:class_expr< [ $list:args$ ] $list:qname$ >> in
      let ct    =
        let h::t = qname in
        let ct   = 
          fold_left 
            (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
            <:class_type< $id:h$ >>  
            t
        in
        <:class_type< $ct$ [ $list:args$ ] >>
      in
      <:class_str_item< inherit $ce$ >>,
      <:class_sig_item< inherit $ct$ >>

    let generate_class_body (_, constrs) = ()

    module M = Map.Make (String)
    
    let m : t M.t ref = ref M.empty

    let register name t =
      if not (M.mem name !m) 
      then m := M.add name t !m

    let get name =
      if not (M.mem name !m) then None else Some (M.find name !m)

    let load_plugins names =
      let load_one name =
        match get name with
        | None ->
          let filename = name ^ ".cmo" in
          let ok = 
            fold_left 
              (fun ok path -> 
                 if not ok then
                   let fullname = Filename.concat path filename in
                   try 
                     loadfile fullname; 
                     true
                   with 
                   | Error (File_not_found _) -> false
                   | Error err -> Pervasives.raise (Bad_plugin (error_message err))
                 else ok         
              ) 
              false 
              !load_path
          in
          if not ok 
          then Pervasives.raise (Bad_plugin (sprintf "Plugin \"%s\" bytecode file not found" name))
          else begin
            match get name with
            | None   -> Pervasives.raise (Bad_plugin (sprintf "Plugin \"%s\" was not properly initialized" name))
            | Some _ -> ()
          end
        | Some _ -> ()
      in
      iter load_one names
      
  end

exception Generic_extension of string

let get_val (VaVal x) = x 

module S = Set.Make (String)

let split3 l = 
  List.fold_right 
    (fun (a, b, c) (x, y, z) -> a::x, b::y, c::z) l ([], [], []) 

let split4 l = 
  List.fold_right 
    (fun (a, b, c, d) (x, y, z, t) -> a::x, b::y, c::z, d::t) l ([], [], [], []) 

let name_generator list =
  let s = ref (fold_right S.add list S.empty) in
  object(self)
    method generate prompt =
      if S.mem prompt !s 
      then self#generate ("_" ^ prompt)
      else (
        s := S.add prompt !s;
        prompt
      )
  end

let rec replace_t loc a n typ =
  let replace_t = replace_t loc a n in 
  let replace_pv lpv = 
    map 
      (function <:poly_variant< `$name$ of $flag:f$ $list:args$ >> -> 
         let args = map replace_t args in
         <:poly_variant< `$name$ of $flag:f$ $list:args$ >>
      ) 
      lpv 
  in
  match typ with
  | <:ctyp< $t1$ as $t2$ >> -> <:ctyp< $replace_t t1$ as $replace_t t2$ >>           
  | <:ctyp< $t1$ $t2$ >> -> 
      (match t1 with
       | <:ctyp< $lid:s$ >> as typ when s = n ->
            let rec inner args t =
              match args, t with
              | [arg], <:ctyp< ' $b$ >> -> if arg = b then <:ctyp< ' $hd a$ >> else typ
              | arg::args, <:ctyp< $t1$ $t2$ >> ->
                  (match t1 with 
                   | <:ctyp< ' $b$ >> when arg = b -> inner args t2
                   | _ -> typ
                  )
              | _ -> typ
              in
              inner a t2
       | _ -> <:ctyp< $replace_t t1$ $replace_t t2$ >>
      )
  | <:ctyp< $t1$ -> $t2$ >> -> <:ctyp< $replace_t t1$ -> $replace_t t2$ >>
  | <:ctyp< ~$s$: $t$ >> -> <:ctyp< ~$s$: $replace_t t$ >>
  | <:ctyp< $t1$ == private $t2$ >> -> <:ctyp< $replace_t t1$ == private $replace_t t2$ >>
  | <:ctyp< $t1$ == $t2$ >> -> <:ctyp< $replace_t t1$ == $replace_t t2$ >>
  | <:ctyp< ?$s$: $t$ >> -> <:ctyp< ?$s$: $replace_t t$ >>
  | <:ctyp< ( $list:lt$ ) >> -> <:ctyp< ( $list:map replace_t lt$ ) >> 
  | <:ctyp< [ > $list:lpv$ ] >> -> <:ctyp< [ > $list:replace_pv lpv$ ] >>
  | <:ctyp< [ < $list:lpv$ ] >> -> <:ctyp< [ < $list:replace_pv lpv$ ] >>
  | <:ctyp< < $list:lst$ > >> -> <:ctyp< < $list:map (fun (s, t) -> s, replace_t t) lst$ > >>
  | <:ctyp< < $list:lst$ .. > >> -> <:ctyp< < $list:map (fun (s, t) -> s, replace_t t) lst$ .. > >>  
  | typ -> typ

let generate t loc =
  let make_call g f args =
    fold_left (fun e a -> <:expr< $e$ $g a$ >>) f args
  in
  let make_fun g args body =
    fold_right 
      (fun arg expr -> <:expr< fun [ $list:[g arg, VaVal None, expr]$ ] >>)                  
      args
      body
  in
  let id x = x in
  let t, d = split   t in
  let t    = flatten t in
  let get_cata =
    let s = fold_left (fun s ((_, n, _), _) -> S.add n s) S.empty d in
    fun name -> 
      if S.mem name s then <:expr< $lid:cata name$ >> 
                      else let name, gcata = <:expr< $lid:name$ >>, <:expr< $lid:"gcata"$ >> in
                           <:expr< $name$ . $gcata$ >>
  in
  let g = 
    let names = 
      fold_left 
        (fun acc ((_, n, d), _) -> 
           let acc = n::acc in
           match d with
           | `Sumi (_, _, types) ->
               fold_left 
                 (fun acc t ->
                    match t with
                    | `Specific (_, [n]) -> n::acc
                    | _ -> acc
                 ) 
                 acc 
                 types
           | _ -> acc
        ) 
        [] 
        d 
    in
    name_generator names
  in
  let trans = g#generate "t"   in
  let ext   = g#generate "ext" in
  let farg  = 
    let module M = Map.Make (String) in
    let m = ref M.empty in
    (fun a -> 
       let p = "f" ^ a in
       try M.find p !m with
         Not_found -> 
           let n = g#generate p in
           m := M.add p n !m;
           n
    ) 
  in
  let subj = g#generate "s"   in
  let acc  = g#generate "acc" in
  let defs =
    map 
      (fun ((args, name, descr), deriving) ->     
         Plugin.load_plugins deriving;
         let of_lid name = <:expr< $lid:name$ >> in
         let orig_typ, closed_typ =
           let make t args =
             fold_left (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >> ) t args
           in
           make <:ctyp< $lid:name$ >> args,
           make <:ctyp< $lid:closed name$ >> args           
         in
         let current     = name in
         let extensible, remove_bound_var, is_bound_var, bound_var = 
           match descr with 
           | `Sumi (x, _, _) | `Poly (`More x, _) -> true, filter (fun s -> s <> x), (fun s -> s = x), Some x
           | _ -> false, (fun x -> x), (fun _ -> false), None
         in
         let polyvar     = match descr with `Poly _ | `Sumi _ -> true | _ -> false in
         let orig_args   = args                     in
         let args        = remove_bound_var args    in
         let generator   = name_generator args      in
         let targs       = map (fun arg -> arg, generator#generate ("t" ^ arg)) args in
         let img name    = assoc name targs         in
         let inh         = generator#generate "inh" in
         let syn         = generator#generate "syn" in
         let class_targs = (match bound_var with None -> [] | Some x -> [x]) @ 
                           (flatten (map (fun (x, y) -> [x; y]) targs)) @ 
                           [inh; syn]              
         in
         let p_descriptor = {
           Plugin.is_polyvar = polyvar;
           Plugin.is_open    = (match bound_var with Some b -> `Yes b | _ -> `No);
           Plugin.type_args  = args;
           Plugin.name       = current;
           Plugin.default    = { 
             Plugin.inh     = <:ctyp< ' $inh$ >>;
             Plugin.syn     = <:ctyp< ' $syn$ >>;
             Plugin.arg_img = (fun a -> <:ctyp< ' $img a$ >>);
           }
         } 
         in
         let derived = map (fun name -> let Some p = Plugin.get name in name, p loc p_descriptor) deriving in
         let tpo_name = generator#generate "tpo" in
         let tpo =
           let methods = 
             map (fun a -> let e = <:expr< $lid:farg a$ >> in <:class_str_item< method $lid:a$ = $e$ >>) args 
           in
           <:expr< object $list:methods$ end >>
         in
         let tpf =
           map (fun a -> 
                  let inh, e, te = <:ctyp< ' $inh$ >>, <:ctyp< ' $a$ >>, <:ctyp< ' $img a$ >> in 
                  fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >>) [inh; e] te 
               )
               args
         in
         let tpt     = <:ctyp< < $list:combine args tpf$ > >> in
         let catype  =
           let gt = 
             let x = <:ctyp< $uid:"GT"$ >> in
             let y = <:ctyp< $lid:"t"$  >> in
             <:ctyp< $x$ . $y$ >> 
           in
           let ft subj = 
             let x = <:ctyp< ' $syn$ >> in
             let y = <:ctyp< $subj$ -> $x$ >> in
             let z = <:ctyp< ' $inh$ >> in
             <:ctyp< $z$ -> $y$ >> 
           in             
           let trt subj =            
             fold_left 
               (fun t ti -> <:ctyp< $t$ $ti$ >>) 
               <:ctyp< # $list:[class_t name]$ >>
               (subj :: (flatten (map (fun a -> [<:ctyp< ' $a$ >>; <:ctyp< ' $img a$ >>]) args)) @ 
                        [<:ctyp< ' $inh$ >>; <:ctyp< ' $syn$ >>]
               )  
           in
           let extt = <:ctyp< $ft orig_typ$ -> $ft orig_typ$ >> in
           let closed_typ =
             if extensible 
             then
               let Some bound_var = bound_var in 
               let b = <:ctyp< ' $bound_var$ >> in
               <:ctyp< $closed_typ$ as $b$ >>
             else closed_typ
           in
           let ft, ft_ext    = ft closed_typ, ft orig_typ in
           let cata_type     = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt closed_typ]) ft in
           let cata_ext_type = fold_right (fun ti t -> <:ctyp< $ti$ -> $t$ >> ) (tpf @ [trt orig_typ; extt]) ft_ext in
           let x = <:ctyp< $gt$ $cata_type$ >> in
           <:ctyp< $x$ $cata_ext_type$ >> 
         in
         let metargs = (map farg args) @ [trans; ext] in
         let args = metargs @ [acc; subj] in
         match descr with
         | `Sumi (_, _, typs) -> 
           let inherits =
             map (function 
                  | `Specific (args, qname) ->
                      let h::tl = args in
                      let args  = 
                        h ::
                        (flatten 
                          (map 
                             (fun a -> 
                                try [a; img a] with 
                                | Not_found -> 
                                    Ploc.raise loc (Generic_extension (sprintf "unbound type variable '%s" a))
                             ) 
                             tl
                          )
                        ) @ 
                        [inh; syn] 
                      in
                      let qname = 
                        let h::t = rev qname in
                        (rev t) @ [class_t h]
                      in
                      let args  = map (fun a -> <:ctyp< ' $a$ >>) args in
                      let ce    = <:class_expr< [ $list:args$ ] $list:qname$ >> in
                      let ct    =
                        let h::t = qname in
                        let ct   = 
                          fold_left 
                            (fun t id -> let id = <:class_type< $id:id$ >> in <:class_type< $t$ . $id$ >>) 
                            <:class_type< $id:h$ >>  
                            t
                        in
                        <:class_type< $ct$ [ $list:args$ ] >>
                      in
                      <:class_str_item< inherit $ce$ >>,
                      <:class_sig_item< inherit $ct$ >>
                 ) 
                 typs
           in
           let inherits, inherits_t = split inherits in
           let class_expr = <:class_expr< object $list:inherits$   end >> in
           let class_type = <:class_type< object $list:inherits_t$ end >> in
           let class_info c = { 
              ciLoc = loc;
              ciVir = Ploc.VaVal true;
              ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
              ciNam = Ploc.VaVal (class_t name);
              ciExp = c
             } 
           in
           let class_def  = <:str_item< class $list:[class_info class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info class_type]$ >> in
           let sum_body  =
             let cases =
               map 
                 (function `Specific (args, qname) ->
                    let expr =
                      let _::t = args  in
                      let args = rev t in
                      let typename =
                        match qname with
                        | [n]  -> <:expr< $lid:n$ >>
                        | h::t -> 
                           let n::t = rev t in
                           let n = <:expr< $lid:n$ >> in
                           let q = 
                             fold_left 
                               (fun q n -> let n = <:expr< $uid:n$ >> in <:expr< $q$ . $n$ >>) 
                               <:expr< $uid:h$ >> 
                               (rev t) 
                           in
                           <:expr< $q$ . $n$ >>
                      in
                      let generic = <:expr< $uid:"GT"$ >> in
                      let cata    = <:expr< $lid:"gcata_ext"$ >> in
                      let func    = <:expr< $typename$ . $generic$ >> in
                      let func    = <:expr< $func$ . $cata$ >> in
                      let ext     = 
                        make_fun id [<:patt< _ >>] <:expr< $lid:"self"$ >> 
                      in
                      make_call id func ((map (fun a -> <:expr< $lid:farg a$>>) args) @ [<:expr< $lid:trans$ >>; ext; <:expr< $lid:acc$ >>; <:expr< $lid:subj$ >>])
                    in
                    let patt =
                      let t::r  = rev qname in
                      let qname = rev (closed t :: r) in
                      let pvt  = <:patt< # $list:qname$ >> in
                      let subj = <:patt< $lid:subj$ >> in
                      <:patt< ( $pvt$ as $subj$ ) >>
                    in
                    patt, VaVal None, expr
                 ) typs
             in
             let local_defs_and_then expr =
               let defs = [<:patt< $lid:"self"$ >>, make_call of_lid <:expr< $lid:cata current$ >> metargs] in
               <:expr< let $list:defs$ in $expr$ >>
             in
             let cases = 
               let last_case =
                 let patt = <:patt< $lid:subj$ >> in
                 let expr = make_call of_lid <:expr< $lid:ext$ >> ["self"; acc; subj] in
                 patt, VaVal None, expr
               in
               cases @ [last_case] 
             in
             let subj  = <:expr< $lid:subj$ >> in
             local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>
           in
           let derived_classes =              
             map (fun (trait, ((prop, _) as dprop)) -> 
                    let Some p = Plugin.get trait in
                    let inherits =
                      map 
                       (function `Specific (b::args, qname) ->
                          let qname, name = 
                            let n::t = rev qname in
                            rev ((trait_t n trait) :: t), n
                          in
                          let descr = {
                            Plugin.is_polyvar = true;
                            Plugin.is_open    = `Yes b;
                            Plugin.type_args  = args;
                            Plugin.name       = name;
                            Plugin.default    = prop;
                          }
                          in
                          Plugin.generate_inherit loc qname descr (p loc descr)
                       ) 
                       typs
                    in
                    let i_def, i_decl = split inherits in
                    let c_def, c_decl = 
                      <:class_expr< object $list:i_def$ end >>, 
                      <:class_type< object $list:i_decl$ end >>
                    in
                    Plugin.generate_classes loc trait p_descriptor dprop (c_def, c_decl)
                 ) 
                 derived
           in
           (<:patt< $lid:cata name$ >>, 
            (make_fun (fun a -> <:patt< $lid:a$ >>) args sum_body)
           ),
           <:sig_item< value $name$ : $catype$ >>,
           (class_def, class_decl) :: derived_classes
                
         | (`Poly _ | `Vari _) as descr -> 
           let get_type_handler, get_local_defs =
             let context = ref [] in
             (fun (args, qname) as typ ->
                let args = match qname with [name] when name = current -> remove_bound_var args | _ -> args in
                let name = 
                  try fst (assoc typ !context) with
                    Not_found ->
                      let compound_name = String.concat "_" (args @ qname) in
                      let base_gcata, ext = 
                        match qname with
                        | [name]      -> get_cata name, [<:expr< $lid:ext$ >>]
                        | name::names -> 
                            let base = 
                              fold_left (fun q name -> 
                                           let name = if Char.lowercase name.[0] = name.[0] 
                                                      then <:expr< $lid:name$ >>
                                                      else <:expr< $uid:name$ >>
                                           in
                                           <:expr< $q$ . $name$ >>   
                                        ) 
                                        <:expr< $uid:name$ >> 
                                        names
                            in
                            let fname = <:expr< $uid:"GT"$ >> in
                            let fname = <:expr< $fname$ . $lid:"gcata"$ >> in
                            <:expr< $base$ . $fname$ >>, []
                      in
                      let impl =
                        (
                         <:patt< $lid:compound_name$ >>, 
                         make_fun 
                           (fun a -> <:patt< $lid:a$ >>)
                           [acc; subj] 
                           (make_call 
                              id 
                              base_gcata 
                              (map of_lid ((map (fun a -> if is_bound_var a then "self" else farg a) args) @ [trans]) @ 
                               ext @ 
                               map of_lid [acc; subj]
                              )
                           )
                        )
                      in
                      let name = <:expr< $lid:compound_name$ >> in
                      context := (typ, (name, impl)) :: !context;
                      name
                in
                name
             ),
             (fun () ->
                map (fun (_, (_, x)) -> x) !context
             )
           in
	   let add_derived_method, get_derived_classes =
             let module M = Map.Make (String) in
             let get k m = try M.find k m with Not_found -> [] in
             let mdef, mdecl = ref M.empty, ref M.empty in
             (fun (cname, cargs) (trait, (_, p_func)) ->
                let prev_def, prev_decl = get trait !mdef, get trait !mdecl in
                let args = fst (fold_right (fun _ (acc, i) -> (sprintf "p%d" i)::acc, i+1) cargs ([], 0)) in
                let constr = {
                  Plugin.constr = cname;
                  Plugin.acc    = "acc";
                  Plugin.subj   = "subj";
                  Plugin.args   = combine args cargs;
                }
                in
                let m_def = 
                  cmethod cname,
                  make_fun (fun a -> <:patt< $lid:a$ >>) ([constr.Plugin.acc; constr.Plugin.subj] @ args) (p_func constr) 
                in
                mdef := M.add trait (m_def::prev_def) !mdef
             ),
             (fun (trait, p) -> 
	        let m_defs = 
                  map (fun (name, body) -> <:class_str_item< method $lid:name$ = $body$ >>) (get trait !mdef) 
                in 
                let i_def, i_decl = Plugin.generate_inherit loc [class_t current] p_descriptor p in
                let ce = <:class_expr< object $list:i_def::m_defs$ end >> in
                let ct = <:class_type< object $list:[]$ end >> in
                Plugin.generate_classes loc trait p_descriptor p (ce, ct)                
	     )
           in
           let match_cases =
             map 
               (fun (cname, cargs) as case -> 
                  iter (add_derived_method case) derived;
                  let args, _ = fold_right (fun arg (acc, n) -> (sprintf "p%d" n) :: acc, n+1) cargs ([], 1) in
                  let args    = rev args in
                  let patt    =
                    fold_left 
                      (fun p id -> let pid = <:patt< $lid:id$ >> in <:patt< $p$ $pid$ >>)
                      (if polyvar then <:patt< ` $cname$ >> else <:patt< $uid:cname$ >>) 
                      args
                  in
                  let met_name = cmethod cname in
                  let met_sig  = 
                    let make_a x y z = 
                      let g  = <:ctyp< $uid:"GT"$ >> in
                      let a  = <:ctyp< $lid:"a"$  >> in
                      let ga = <:ctyp< $g$ . $a$  >> in
                      let ga = <:ctyp< $ga$ $x$   >> in
                      let ga = <:ctyp< $ga$ $y$   >> in
                      let ga = <:ctyp< $ga$ $z$   >> in
                      <:ctyp< $ga$ $tpt$ >>                           
                    in
                    let make_typ = function
                    | `Generic   t    -> t
                    | `Variable    name -> make_a <:ctyp< ' $inh$ >> <:ctyp< ' $name$ >> <:ctyp< ' $img name$ >>
                    | `Specific (targs, qname) ->   
                         let typ =
                           let qtype =
                             match rev qname with
                             | name::qname -> 
                                fold_right 
                                  (fun a acc -> let t = <:ctyp< $uid:a$ >> in <:ctyp< $t$ . $acc$ >>) 
                                  qname 
                                  <:ctyp< $lid: name$ >>
                           in
                           fold_left 
                             (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) 
                             qtype 
                             targs
                         in
                         make_a <:ctyp< ' $inh$ >> typ <:ctyp< ' $syn$ >>
                    in
                    let typs = [<:ctyp< ' $inh$ >>; make_typ (`Specific (orig_args, [name]))] @ (map make_typ cargs) in
                    fold_right (fun t s -> <:ctyp< $t$ -> $s$ >> ) typs <:ctyp< ' $syn$ >>
                  in
                  let expr =
                    let obj      = <:expr< $lid:trans$ >>        in
                    let met      = <:expr< $obj$ # $met_name$ >> in
                    let garg f x =
                      let g = <:expr< $uid:"GT"$ >> in
                      let m = <:expr< $lid:"make"$ >> in
                      let gm = <:expr< $g$ . $m$ >> in
                      make_call id gm [f; x; <:expr< $lid:tpo_name$ >>]
                    in
                    make_call id 
                      met 
                       (<:expr< $lid:acc$  >> :: 
                         (garg <:expr< $lid:"self"$ >> <:expr< $lid:subj$ >>) :: 
                         (map (fun (typ, x) -> 
                                 match typ with
                                 | `Generic _   -> <:expr< $lid:x$ >>
                                 | `Variable name -> garg <:expr< $lid:farg name$ >> <:expr< $lid:x$ >>
                                 | `Specific t   -> 
                                     let name = get_type_handler t in 
                                     garg name <:expr< $lid:x$ >>
                              ) 
                              (combine cargs args)
                         )
                       )
                  in
                  (patt, VaVal None, expr), 
                  [met_name, met_sig],
                  [<:class_str_item< method virtual $lid:met_name$ : $met_sig$ >>], 
                  [<:class_sig_item< method virtual $lid:met_name$ : $met_sig$ >>]
               ) 
               (match descr with `Vari cons | `Poly (_, cons) -> cons)
           in
           let match_cases = 
             if extensible then match_cases @ [(<:patt< $lid:others$ >>, 
                                                VaVal None,
                                                make_call of_lid <:expr< $lid:ext$ >> ["self"; acc; others]
                                               ),
                                               [],
                                               [],
                                               []
                                              ]
                           else match_cases
           in
           let subj = <:expr< $lid:subj$ >> in 
           let local_defs_and_then expr =
             let local_defs =
                get_local_defs () @
                [<:patt< $lid:"self"$ >>  , make_call of_lid <:expr< $lid:cata current$ >> metargs;
                 <:patt< $lid:tpo_name$ >>, tpo
                ]                                                   
             in
             match local_defs with
             | [] -> expr
             | _  -> <:expr< let $list:local_defs$ in $expr$ >>
           in
           let cases, _, methods, methods_sig = split4 match_cases in
           let methods      = flatten methods      in
           let methods_sig  = flatten methods_sig  in
           let class_expr   = <:class_expr< object $list:methods$     end >> in
           let class_type   = <:class_type< object $list:methods_sig$ end >> in
           let class_info c = { 
              ciLoc = loc;
              ciVir = Ploc.VaVal true;
              ciPrm = (loc, Ploc.VaVal (map (fun a -> Ploc.VaVal (Some a), None) class_targs));
              ciNam = Ploc.VaVal (class_t name);
              ciExp = c;
             } 
           in
           let class_def  = <:str_item< class $list:[class_info class_expr]$ >> in
           let class_decl = <:sig_item< class $list:[class_info class_type]$ >> in 
           (<:patt< $lid:cata name$ >>, 
            (make_fun (fun a -> <:patt< $lid:a$ >>) args (local_defs_and_then <:expr< match $subj$ with [ $list:cases$ ] >>))
           ),
           <:sig_item< value $name$ : $catype$ >>,
           [class_def, class_decl] @ (map get_derived_classes derived)
      ) 
      d
  in
  let generic_cata = 
    let g = <:patt< $uid:"GT"$ >> in
    let c = <:patt< $lid:"gcata"$ >> in
    <:patt< $g$ . $c$ >>
  in
  let generic_cata_ext = 
    let g = <:patt< $uid:"GT"$ >> in
    let c = <:patt< $lid:"gcata_ext"$ >> in
    <:patt< $g$ . $c$ >>
  in
  let pnames, tnames = 
    split (
      map (fun ((args, name, descr), deriving) -> 
             let p = snd (fold_left (fun (i, acc) _ -> i+1, (sprintf "p%d" i)::acc) (0, []) (["t"; "acc"; "s"] @ args)) in
             let pe = [
               generic_cata_ext, <:expr< $lid:cata name$ >>; 
               generic_cata, let ext, p = 
                               match descr with 
                               | `Poly (`More _, _) | `Sumi _ -> 
                                    let p = tl p in
                                    let px = <:patt< $lid:"x"$ >> in
                                    let tx = 
                                      fold_left 
                                        (fun t a -> let a = <:ctyp< ' $a$ >> in <:ctyp< $t$ $a$ >>) 
                                        <:ctyp< $lid:closed name$ >> 
                                        args
                                    in                                        
                                    make_fun 
                                      id 
                                      [<:patt< $lid:"f"$ >>; <:patt< $lid:"acc"$ >>; <:patt< ( $px$ : $tx$ ) >>] 
                                      (make_call id 
                                         <:expr< $lid:"f"$ >> 
                                         [<:expr< $lid:"acc"$ >>; <:expr< $lid:"x"$ >>]
                                      ), p

                               | _ -> 
                                    let g = <:expr< $uid:"GT"$ >> in
                                    let a = <:expr< $lid:"apply"$ >> in
                                    <:expr< $g$ . $a$ >>, p
                             in                                 
                             let args =
                                let x :: y :: a = rev (map (fun arg -> <:expr< $lid:arg$ >>) p) in
                                rev a @ [ext; y; x]
                             in
                             let cata = <:expr< $lid:cata name$ >> in
                             make_fun (fun a -> <:patt< $lid:a$ >>) p (make_call id cata args)
             ] 
             in 
             <:patt< $lid:name$ >>, <:expr< { $list:pe$ } >>
          ) 
          d
    ) 
  in
  let tuple = <:patt< ( $list:pnames$ ) >> in
  let tup = <:expr< ( $list:tnames$ ) >> in 
  let defs, decls, classes = split3 defs in
  let class_defs, class_decls = split (flatten classes) in
  let def = <:expr< let rec $list:defs$ in $tup$ >> in
  let cata_def  = <:str_item< value $list:[tuple, def]$ >> in
  let type_def  = <:str_item< type $list:t$ >> in
  let type_decl = <:sig_item< type $list:t$ >> in
  <:str_item< declare $list:[type_def; cata_def] @ class_defs$ end >>,
  <:sig_item< declare $list:[type_decl] @ class_decls @ decls$ end >> 
    
EXTEND
  GLOBAL: sig_item str_item ctyp class_expr class_longident; 

  ctyp: LEVEL "ctyp2" [
    [ "@"; t=ctyp LEVEL "ctyp2"-> 
      let rec inner = function
      | <:ctyp< $q$ . $t$ >> -> <:ctyp< $q$ . $inner t$ >>
      | <:ctyp< $t$ $a$ >> -> <:ctyp< $inner t$ $a$ >>
      | <:ctyp< $lid:name$ >> -> <:ctyp< $lid:closed name$ >>
      | t -> Ploc.raise loc (Generic_extension "application or qualified name expected")
      in
      inner t
    ]
  ];
  
  class_longident: [
    [ "@"; ci=qname; t=OPT trait -> 
      let n::q = rev (snd ci) in      
      rev ((match t with None -> class_t n | Some t -> trait_t n t)::q) 
    ]
  ];

  trait: [[ "["; id=LIDENT; "]" -> id ]];

  str_item: LEVEL "top" [
    [ "generic"; t=LIST1 t_decl SEP "and" -> fst (generate t loc) ]
  ];

  sig_item: LEVEL "top" [
    [ "generic"; t=LIST1 t_decl SEP "and" -> snd (generate t loc) ]
  ];

  t_decl: [
    [ a=fargs; n=LIDENT; "="; t=rhs ->
      let (is_private, ((def, cons), t)), deriving = t in
      let descriptor, types =
        match t with
        | `Sumi (var, lpv, _) -> 
           ignore (
             match a with
             | a::_ when a = var -> ()
             | _ -> Ploc.raise loc (Generic_extension (sprintf "sum type must be polymorphic with the first type variable '%s" var))
           );
           (a, n, t),
           [{
             tdNam = VaVal (loc, VaVal (closed n));
             tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
             tdPrv = VaVal false;
             tdDef = replace_t loc a n def; 
             tdCon = VaVal []            
            };
            {
             tdNam = VaVal (loc, VaVal n);
             tdPrm = VaVal (map (fun name -> VaVal (Some name), None) a);
             tdPrv = VaVal false;
             tdDef = <:ctyp< ' $var$ >>; 
             tdCon = VaVal [<:ctyp< ' $var$ >>, <:ctyp< [ > $list:lpv$ ] >>]
            }]

        | `Poly (`More b, d) ->
           (match a with 
           | f::_ when f <> b -> Pervasives.raise (Generic_extension (sprintf "type argument \"%s\" should be listed first in type \"%s\" definition." b n))
           | [] -> Pervasives.raise (Generic_extension (sprintf "type \"%s\" should atleast have type argument \"%s\"." n b))
           | _  -> (a, n, t)
           ),
           (let lcons =
              map 
                (fun (constr, args) ->                     
                   match args with
                   | [] -> <:poly_variant< `$constr$ >>
                   | _  ->
                      let args = 
                        map 
                          (function 
                           | `Generic t -> replace_t loc a n t
                           | `Specific (targs, [name]) when name = n && targs = a -> <:ctyp< ' $hd a$ >>
                           | `Specific (targs, qname) ->   
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
                           | `Variable a -> <:ctyp< ' $a$ >>
                          )
                          args
                      in
                      let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in 
                      <:poly_variant< `$constr$ of $flag:false$ $list:args$ >>
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
    ]
  ];

  rhs: [[b=rhs_base; d=OPT deriving -> b, match d with None -> [] | Some d -> d]];

  deriving: [["deriving"; s=LIST1 LIDENT SEP "," -> s]];

  rhs_base: [[ vari ] | [ poly ]];
  
  vari: [
    [ p=OPT "private"; OPT "|"; vari_cons=LIST1 vari_con SEP "|" -> 
        let x, y = split vari_cons in
        (p <> None), ((<:ctyp< [ $list:x$ ] >>, []), `Vari y)
    ]
  ];

  vari_con: [
    [ c=UIDENT; a=con_args -> (loc, VaVal c, fst a, None), (c, snd a) ]
  ];

  poly: [
    [ "["; ">"; body=poly_body; "]"; "as"; a=targ ->
        false,
        match body with
        | `TypeDef (lcons, y) ->
            (<:ctyp< ' $a$ >>, [<:ctyp< ' $a$ >>, <:ctyp< [ > $list:lcons$ ] >>]), `Poly (`More a, y)
        | `TypeSum s -> s
    ] |
    [ "["; body=poly_body; "]" ->
        false, 
        match body with
        | `TypeDef (lcons, y) ->
           (<:ctyp< [ = $list:lcons$ ] >>, []), `Poly (`Equal, y) 
        | `TypeSum s -> s
    ]    
  ];

  poly_body: [
    [ OPT "|"; poly_cons=LIST1 poly_con SEP "|" ->
        let x, y = split poly_cons in
        let lcons = map (fun (loc, name, args, _) ->                                  
	    	    	     	 if length args = 0 
                                 then <:poly_variant< `$name$ >>         
                                 else                          
                                   let args = if length args = 1 then args else [<:ctyp< ($list:args$) >>] in
                                   <:poly_variant< `$name$ of $flag:false$ $list:args$ >> 
                              ) x
        in
        `TypeDef (lcons, y)
    ] |
    [ OPT "|"; typs=LIST1 c_typ SEP "|" ->
      let t, d = split typs in
      let a = ref None in
      let d = 
        map
          (fun d -> 
             match d with
             | `Variable x -> 
                 Ploc.raise loc (Generic_extension (sprintf "type variable ('%s) is not allowed in type sum" x))
             | `Specific ([], _) ->
                 Ploc.raise loc (Generic_extension "polymorphic type expected in type sum")
             | `Specific (b::_ as args, qname) ->
                 (match !a with 
                 | None -> a := Some b
                 | Some a when a <> b -> 
                    Ploc.raise loc (Generic_extension (sprintf "type variable '%s should be the first parameter of all types this type sum" a))
                 | _ -> ()
                 );
                 `Specific (args, qname) 
          ) 
          d
      in
      let Some a = !a in
      let t = 
        map 
          (fun t ->
             let rec replace = function
             | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $replace t1$ $t2$ >>
             | <:ctyp< $t1$ . $t2$ >> -> <:ctyp< $t1$ . $replace t2$ >>
             | <:ctyp< $lid:n$ >> -> <:ctyp< $lid:closed n$ >>
             | t -> t
             in 
             <:poly_variant< $replace t$ >>
          ) 
          t 
      in
      `TypeSum ((<:ctyp< [= $list:t$ ] >>, []), `Sumi (a, t, d))
    ]
  ];

  poly_con: [
    [ "`"; c=UIDENT; a=con_args -> (loc, c, get_val (fst a), None), (c, snd a) ]
  ];

  con_args: [
    [ "of"; a=LIST1 typ SEP "*" -> let x, y = split a in VaVal x, y ] |
    [ -> VaVal [], [] ]
  ];

  typ: [ 
    [ "["; t=c_typ; "]" -> 
      let t, d = t in
      t, (d :> [`Specific of string list * string list | `Variable of string | `Generic of ctyp])
    ] |
    [ t=ctyp LEVEL "apply" -> t, `Generic t ]
  ];

  c_typ: [
    [ a=targ; q=OPT qname -> 
       let at = <:ctyp< ' $a$ >> in
       match q with 
       | Some q -> <:ctyp< $fst q$ $at$ >>, `Specific ([a], snd q)
       | None -> <:ctyp< ' $a$ >>, `Variable a
    ] | 
    [ "("; a=LIST1 targ SEP ","; ")"; q=qname -> 
       fold_left (fun acc a -> let at = <:ctyp< ' $a$ >> in <:ctyp< $acc$ $at$ >>) (fst q) a, 
       `Specific (a, snd q) 
    ] |
    [ q=qname -> fst q, `Specific ([], snd q) ]
  ];

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

  qualifier: [
    [ x=UIDENT; "." -> x ]
  ];

  fargs: [
    [ a=targ -> [a] ] |
    [ "("; a=LIST1 targ SEP ","; ")" -> a ] |
    [ -> [] ]
  ];

  targ: [
    [ "'"; a=LIDENT -> a ]
  ];
  
END;
