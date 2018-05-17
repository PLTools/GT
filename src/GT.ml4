 (**************************************************************************
 *  Copyright (C) 2012-2015
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

type ('a, 'b) t = {gcata : 'a; plugins : 'b}
type ('a, 'b, 'c, 'd) a = {x : 'b; fx : 'a -> 'c; f : 'a -> 'b -> 'c; t : 'd}

let (~:) x = x.x
let transform t = t.gcata

let make  f x p = {x=x; fx=(fun a -> f a x); f=f; t=p}
let apply f a x = f a x

let lift f _ = f

type comparison = LT | EQ | GT

let chain_compare x f =
  match x with
  | EQ -> f ()
  | _  -> x

let compare_primitive x y =
  if x < y
  then LT
  else if x > y
       then GT
       else EQ

let poly_tag x =
  let x = Obj.magic x in
  (Obj.magic (if Obj.is_block x then Obj.field x 0 else x) : int)

let vari_tag x =
  if Obj.is_block x then Obj.tag x else Obj.magic x

let compare_poly x y =
  compare_primitive (poly_tag x) (poly_tag y)

let compare_vari x y =
  let x, y = Obj.magic x, Obj.magic y in
  match compare_primitive (Obj.is_block x) (Obj.is_block y) with
  | EQ -> compare_primitive (vari_tag x) (vari_tag y)
  | c  -> x

let string_of_string  s = "\"" ^ String.escaped s ^ "\""
let string_of_unit    _ = "()"
let string_of_char    c = String.make 1 c
let string_of_int32     = Int32.to_string
let string_of_int64     = Int64.to_string
let string_of_nativeint = Nativeint.to_string

GENERIFY(bool)
GENERIFY(int)
GENERIFY(string)
GENERIFY(char)
GENERIFY(unit)
GENERIFY(int32)
GENERIFY(int64)
GENERIFY(nativeint)

type 'a plist      = 'a list
type 'a list       = 'a plist

class type html_list_env_tt = object  end
class type show_list_env_tt = object  end
class type foldl_list_env_tt = object  end
class type foldr_list_env_tt = object  end
class type eq_list_env_tt = object  end
class type compare_list_env_tt = object  end
class type gmap_list_env_tt = object  end

class type ['a, 'ia, 'sa, 'inh, 'syn] list_tt =
  object
    method c_Nil  : 'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method c_Cons : 'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
                                    ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a ->
                                    ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_list : ('ia -> 'a -> 'sa) -> 'inh -> 'a list -> 'syn
  end

let list : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #list_tt -> 'inh -> 'a list -> 'syn, unit) t =
  let rec list_gcata fa trans inh subj =
    let rec self = list_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      [] -> trans#c_Nil inh (make self subj tpo)
    | p1::p2 ->
        trans#c_Cons inh (make self subj tpo) (make fa p1 tpo)
          (make self p2 tpo)
  in
  {gcata = list_gcata; plugins = ()}

class virtual ['a, 'ia, 'sa, 'inh, 'syn] list_t =
  object (this)
    method virtual c_Nil :
      'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method virtual c_Cons :
      'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a ->
        ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_list fa = transform list fa this
  end

class ['a] html_list_t =
  object
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] @list
    method c_Nil  _ _      = View.empty
    method c_Cons _ _ x xs = View.concat (x.fx ()) (match xs.x with [] -> View.empty | _ -> HTML.li (xs.fx ()))
  end

class ['a] show_list_t =
  object
    inherit ['a, unit, string, unit, string] @list
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = x.fx () ^ (match xs.x with [] -> "" | _ -> "; " ^ xs.fx ())
  end

class ['a, 'sa] gmap_list_t =
  object
    inherit ['a, unit, 'sa, unit, 'sa list] @list
    method c_Nil _ _ = []
    method c_Cons _ _ x xs = x.fx () :: xs.fx ()
  end

class ['a, 'syn] foldl_list_t =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn] @list
    method c_Nil s _ = s
    method c_Cons s _ x xs = xs.fx (x.fx s)
  end

class ['a, 'syn] foldr_list_t =
  object
    inherit ['a, 'syn] @list[foldl]
    method c_Cons s _ x xs = x.fx (xs.fx s)
  end

class ['a] eq_list_t =
  object
    inherit ['a, 'a, bool, 'a list, bool] @list
    method c_Nil inh subj = inh = []
    method c_Cons inh subj x xs =
      match inh with
      | y::ys -> x.fx y && xs.fx ys
      | _ -> false
  end

class ['a] compare_list_t =
  object
    inherit ['a, 'a, comparison, 'a list, comparison] @list
    method c_Nil inh subj =
      match inh with
      | [] -> EQ
      |  _ -> GT
    method c_Cons inh subj x xs =
      match inh with
      | [] -> LT
      | (y::ys) ->
	  (match x.fx y with
	  | EQ -> xs.fx ys
	  | c  -> c
	  )
  end

let list : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #list_tt -> 'inh -> 'a list -> 'syn,
            < show    : ('a -> string)      -> 'a list -> string;
              html    : ('a -> HTML.viewer) -> 'a list -> HTML.viewer;
              gmap    : ('a -> 'b) -> 'a list -> 'b list;
              foldl   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              foldr   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              eq      : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool;
              compare : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison;
            >) t =
  {gcata   = list.gcata;
   plugins = object
               method show    fa l = "[" ^ (transform(list) (lift fa) (new @list[show]) () l) ^ "]"
               method html    fa   = transform(list) (lift fa) (new @list[html]) ()
               method gmap    fa   = transform(list) (lift fa) (new @list[gmap] ) ()
               method eq      fa   = transform(list) fa (new @list[eq])
               method compare fa   = transform(list) fa (new @list[compare])
               method foldl   fa   = transform(list) fa (new @list[foldl])
               method foldr   fa   = transform(list) fa (new @list[foldr])
             end
  }

module Lazy =
  struct

    type ('a, 'b) t' = ('a, 'b) t

    include Lazy

    class type html_t_env_tt = object  end
    class type show_t_env_tt = object  end
    class type foldl_t_env_tt = object  end
    class type foldr_t_env_tt = object  end
    class type eq_t_env_tt = object  end
    class type compare_t_env_tt = object  end
    class type gmap_list_env_tt = object  end

    class type ['a, 'ia, 'sa, 'inh, 'syn] t_tt =
      object
        method t_t : ('ia -> 'a -> 'sa) -> 'inh -> 'a t -> 'syn
      end

    let t : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #t_tt -> 'inh -> 'a t -> 'syn, unit) t' =
      let t_gcata fa trans inh subj = trans#t_t fa inh subj (* fa inh (Lazy.force subj) in*) in
      {gcata = t_gcata; plugins = ()}

    class virtual ['a, 'ia, 'sa, 'inh, 'syn] t_t =
      object (this)
        method virtual t_t : ('ia -> 'a -> 'sa) -> 'inh -> 'a t -> 'syn
      end

    class ['a] html_t_t =
      object
        inherit ['a, unit, HTML.viewer, unit, HTML.viewer] @t
        method t_t fa inh subj = fa inh @@ Lazy.force subj
      end

    class ['a] show_t_t =
      object
        inherit ['a, unit, string, unit, string] @t
        method t_t fa inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'sa] gmap_t_t =
      object
        inherit ['a, unit, 'sa, unit, 'sa t] @t
        method t_t fa inh subj = lazy (fa inh @@ Lazy.force subj)
      end

    class ['a, 'syn] foldl_t_t =
      object
        inherit ['a, 'syn, 'syn, 'syn, 'syn] @t
        method t_t fa inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'syn] foldr_t_t =
      object
        inherit ['a, 'syn] @t[foldl]
      end

    class ['a] eq_t_t =
      object
        inherit ['a, 'a, bool, 'a t, bool] @t
        method t_t fa inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end


    class ['a] compare_t_t =
      object
        inherit ['a, 'a, comparison, 'a t, comparison] @t
        method t_t fa inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    let t : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #t_tt -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      -> 'a t -> string;
               html    : ('a -> HTML.viewer) -> 'a t -> HTML.viewer;
               gmap    : ('a -> 'b) -> 'a t -> 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >) t' =
      {gcata   = t.gcata;
       plugins = object
                   method show    fa l = transform(t) (lift fa) (new @t[show]) () l
                   method html    fa   = transform(t) (lift fa) (new @t[html]) ()
                   method gmap    fa   = transform(t) (lift fa) (new @t[gmap] ) ()
                   method eq      fa   = transform(t) fa (new @t[eq])
                   method compare fa   = transform(t) fa (new @t[compare])
                   method foldl   fa   = transform(t) fa (new @t[foldl])
                   method foldr   fa   = transform(t) fa (new @t[foldr])
                 end
      }
  end

type 'a poption = 'a option
type 'a option = 'a poption

class type html_option_env_tt = object  end
class type show_option_env_tt = object  end
class type foldl_option_env_tt = object  end
class type foldr_option_env_tt = object  end
class type eq_option_env_tt = object  end
class type compare_option_env_tt = object  end
class type gmap_option_env_tt = object  end

class type ['a, 'ia, 'sa, 'inh, 'syn] option_tt =
  object
    method c_None : 'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method c_Some : 'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
                            ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_option : ('ia -> 'a -> 'sa) -> 'inh -> 'a option -> 'syn
  end

let option : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #option_tt -> 'inh -> 'a option -> 'syn, unit) t =
  let rec option_gcata fa trans inh subj =
    let rec self = option_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      None   -> trans#c_None inh (make self subj tpo)
    | Some p -> trans#c_Some inh (make self subj tpo) (make fa p tpo)
  in
  {gcata = option_gcata; plugins = ()}

class virtual ['a, 'ia, 'sa, 'inh, 'syn] option_t =
  object (this)
    method virtual c_None :
      'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method virtual c_Some :
      'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_option fa = transform option fa this
  end

class ['a] html_option_t =
  object
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] @option
    method c_None  _ _  = HTML.string "None"
    method c_Some _ _ x = View.concat (HTML.string "Some") (HTML.ul (x.fx ()))
  end

class ['a] show_option_t =
  object
    inherit ['a, unit, string, unit, string] @option
    method c_None  _ _  = "None"
    method c_Some _ _ x = "Some (" ^ x.fx () ^ ")"
  end

class ['a, 'sa] gmap_option_t =
  object
    inherit ['a, unit, 'sa, unit, 'sa option] @option
    method c_None _ _ = None
    method c_Some _ _ x = Some (x.fx ())
  end

class ['a, 'syn] foldl_option_t =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn] @option
    method c_None s _ = s
    method c_Some s _ x = x.fx s
  end

class ['a, 'syn] foldr_option_t =
  object
    inherit ['a, 'syn] @option[foldl]
  end

class ['a] eq_option_t =
  object
    inherit ['a, 'a, bool, 'a option, bool] @option
    method c_None inh subj = inh = None
    method c_Some inh subj x =
      match inh with
      | Some y -> x.fx y
      | _ -> false
  end

class ['a] compare_option_t =
  object
    inherit ['a, 'a, comparison, 'a option, comparison] @option
    method c_None inh subj =
      match inh with
      | None -> EQ
      | _  -> GT
    method c_Some inh subj x =
      match inh with
      | None -> LT
      | Some y -> x.fx y
  end

let option : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #option_tt -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
                html    : ('a -> HTML.viewer) -> 'a option -> HTML.viewer;
                gmap    : ('a -> 'b) -> 'a option -> 'b option;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool;
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison;
              >) t =
  {gcata   = option.gcata;
   plugins = object
               method show    fa = transform(option) (lift fa) (new @option[show]) ()
               method html    fa = transform(option) (lift fa) (new @option[html]) ()
               method gmap    fa = transform(option) (lift fa) (new @option[gmap] ) ()
               method eq      fa = transform(option) fa (new @option[eq])
               method compare fa = transform(option) fa (new @option[compare])
               method foldl   fa = transform(option) fa (new @option[foldl])
               method foldr   fa = transform(option) fa (new @option[foldr])
             end
  }

type ('a, 'b) pair = 'a * 'b

class type html_pair_env_tt = object  end
class type show_pair_env_tt = object  end
class type foldl_pair_env_tt = object  end
class type foldr_pair_env_tt = object  end
class type eq_pair_env_tt = object  end
class type compare_pair_env_tt = object  end
class type gmap_pair_env_tt = object  end

(*
class type ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] pair_tt =
  object
    method c_Pair : 'inh -> ('inh, ('a, 'b) pair, 'syn, < a : 'ia -> 'a -> 'sa; b : 'ib -> 'b -> 'sb >) a ->
                            ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa ; b : 'ib -> 'b -> 'sb >) a ->
                            ('ib, 'b, 'sb, < a : 'ia -> 'a -> 'sa ; b : 'ib -> 'b -> 'sb >) a -> 'syn
    method t_pair : ('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> 'inh -> ('a, 'b) pair -> 'syn
  end
*)

let pair_gcata tr inh = function (a, b) -> tr#c_Pair inh a b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pair =
  object (this)
    method virtual c_Pair : 'inh -> 'a -> 'b -> 'syn
    (* method t_pair fa fb = transform pair fa fb this *)
  end


class ['a, 'b, 'extra] show_pair _ fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string, 'extra] class_pair
    method c_Pair () x y = "(" ^ fa x ^ ", " ^ fb y ^ ")"
  end
class ['a, 'b, 'extra] html_pair _ fa fb =
  object
    inherit ['a, unit, 'syn, 'b, unit, 'syn, unit, 'syn, 'extra] class_pair
    constraint 'syn = HTML.viewer
    method c_Pair () x y = HTML.string "not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'extra] gmap_pair _ (fa: 'a -> 'sa) fb =
  object
    inherit ['a, unit, 'sa, 'b, unit, 'sb, unit, ('sa, 'sb) pair, 'extra] class_pair
    method c_Pair () x y = (fa x, fb y)
  end

class ['a, 'b, 'syn, 'extra] foldl_pair _ fa fb  =
  object
    inherit ['a, 'syn, 'syn, 'b, 'syn, 'syn, 'syn, 'syn, 'extra] class_pair
    method c_Pair s x y = fb (fa s x) y
  end

class ['a, 'b, 'syn, 'extra] foldr_pair _ fa fb  =
  object
    inherit ['a, 'syn, 'syn, 'b, 'syn, 'syn, 'syn, 'syn, 'extra] class_pair
    method c_Pair s x y = fa (fb s y) x
  end

class ['a, 'b, 'extra] eq_pair _ fa fb  =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) pair, bool, 'extra] class_pair
    method c_Pair inh x y =
      match inh with
      (z, t) -> fa z x && fb t y
  end

class ['a, 'b, 'extra] compare_pair _ fa fb  =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) pair, 'syn, 'extra] class_pair
    constraint 'syn = comparison
    method c_Pair inh x y =
      match inh with
       (z, t) -> (match fa z x with EQ -> fb t y | c -> c)
  end

(*
class ['a, 'b, 'syn, 'extra] foldr_pair_t _ fa fb  =
  object
    inherit ['a, 'b, 'syn, 'extra] @pair[foldl]
    method c_Pair s _ x y = y.fx (x.fx s)
  end

class ['a, 'b, 'extra] html_pair_t =
  object
    inherit ['a, unit, HTML.viewer, 'b, unit, HTML.viewer, unit, HTML.viewer, 'extra] @pair
    method c_Pair _ _ x y =
      List.fold_left View.concat View.empty
         [HTML.string "("; HTML.ul (x.fx ()); HTML.string ", "; HTML.ul (y.fx ()); HTML.string ")"]
  end
*)

let pair : ( ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn,_) #class_pair -> 'inh -> ('a, 'b) pair -> 'syn,
              < show    : ('a -> string) -> ('b -> string) -> ('a, 'b) pair -> string;
                html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) -> ('a, 'b) pair -> HTML.viewer;
                gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) pair -> ('c, 'd) pair;
                foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) pair -> 'c;
                foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) pair -> 'c;
                eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) ->
                          ('a, 'b) pair -> ('a, 'b) pair -> bool;
                compare : ('a -> 'a -> comparison) -> ('b -> 'b -> comparison) ->
                          ('a, 'b) pair -> ('a, 'b) pair -> comparison;
              >) t =
  {gcata   = pair_gcata;
   plugins = object
       method show    fa fb = pair_gcata (new show_pair (fun _ -> assert false) fa fb) ()
       method html    fa fb = pair_gcata (new html_pair (fun _ -> assert false) fa fb) ()
       method gmap    fa fb = pair_gcata (new gmap_pair (fun _ -> assert false) fa fb) ()
       method eq      fa fb = pair_gcata (new eq_pair     (fun _ -> assert false) fa fb)
       method compare fa fb = pair_gcata (new compare_pair(fun _ -> assert false) fa fb)
       method foldl   fa fb = pair_gcata (new foldl_pair  (fun _ -> assert false) fa fb)
       method foldr   fa fb = pair_gcata (new foldr_pair  (fun _ -> assert false) fa fb)

(*
               method html    fa fb = pair_gcata (lift fa) (lift fb) (new @pair[html]) ()
               method foldr   fa fb = pair_gcata (new foldr_pair  fa fb)
*)
  end
  }

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_tuple2 = object
  inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pair
end
let gcata_tuple2 = pair.gcata

class ['a, 'b, 'extra] show_tuple2 fself fa fb = object
  inherit [ 'a, 'b, 'extra] show_pair fself fa fb
end
class ['a, 'a2, 'b, 'b2, 'extra] gmap_tuple2 fself fa fb = object
  inherit [ 'a, 'a2, 'b, 'b2, 'extra] gmap_pair fself fa fb
end
class ['a, 'b, 'extra] compare_tuple2 fself fa fb = object
  inherit [ 'a, 'b, 'extra] compare_pair fself fa fb
end
class ['a, 'b, 'extra] eq_tuple2 fself fa fb = object
  inherit [ 'a, 'b, 'extra] eq_pair fself fa fb
end

type ('a,'b,'c) triple = 'a * 'b * 'c
class virtual ['a,'ia,'sa, 'b,'ib,'sb, 'c,'ic,'sc, 'inh, 'syn] triple_t = object
  method virtual c_Triple : 'inh -> 'a -> 'b -> 'c -> 'syn
end
class ['a, 'b, 'c, 'extra] show_triple _ fa fb fc =
  object
    inherit [ 'a, unit, string
            , 'b, unit, string
            , 'c, unit, string
            , unit, string] @triple
    method c_Triple () x y z = Printf.sprintf "(%s, %s, %s)"
      (fa x) (fb y) (fc z)
end
class ['a, 'a2, 'b, 'b2,  'c, 'c2, 'extra] gmap_triple _ fa fb fc =
  object
    inherit [ 'a, unit, 'a2
            , 'b, unit, 'b2
            , 'c, unit, 'c2
            , unit, ('a2,'b2,'c2) triple ] @triple
    method c_Triple () x y z = ( (fa x), (fb y), (fc z) )
end
class ['a, 'b, 'c, 'extra] compare_triple _ fa fb fc =
  object
    inherit [ 'a, 'a, 'syn
            , 'b, 'b, 'syn
            , 'c, 'c, 'syn
            , 'inh, 'syn ] @triple
    constraint 'inh = ('a, 'b, 'c) triple
    constraint 'syn = comparison
    method c_Triple (a,b,c) x y z =
      chain_compare (fa a x) @@ fun _ ->
      chain_compare (fb b y) @@ fun _ ->
       (fc c z)

end
let triple_gcata tr inh (a,b,c) = tr#c_Triple inh a b c

let triple :
    ( (* ('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> ('ic -> 'c -> 'sc) -> *)
             ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'c, 'ic, 'sc, 'inh, 'syn) #triple_t ->
             'inh -> ('a, 'b, 'c) triple -> 'syn
    , < show    : ('a -> string) -> ('b -> string) ->  ('c -> string) ->
                       ('a, 'b, 'c) triple  -> string;
        gmap    : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) ->
                       ('a, 'b, 'c) triple -> ('d, 'e, 'f) triple;
        compare : ('a -> 'a -> comparison) ->
                  ('b -> 'b -> comparison) ->
                  ('c -> 'c -> comparison) ->
                  ('a, 'b, 'c) triple ->
                  ('a, 'b, 'c) triple ->
                  comparison
      >) t =
  {gcata   = triple_gcata;
   plugins = object
     method show    fa fb fc =
       triple_gcata (new show_triple    (fun _ -> assert false) fa fb fc) ()
     method gmap    fa fb fc =
       triple_gcata (new gmap_triple    (fun _ -> assert false) fa fb fc) ()
     method compare fa fb fc inh =
       triple_gcata (new compare_triple (fun _ -> assert false) fa fb fc) inh
  end
}

let tuple2 = pair
let tuple3 = triple

(****************************************************************************)
let show    t = t.plugins#show
let html    t = t.plugins#html
let gmap    t = t.plugins#gmap
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare


let fix0 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t