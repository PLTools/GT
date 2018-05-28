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
let transform t = t.gcata

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

(* Fixpoint combinator to define recursive transformation without extra
 * object allocations *)
let fix0 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t


(** Standart type go there *)
type 'a plist      = 'a list
type 'a list       = 'a plist

class virtual ['a, 'ia, 'sa, 'inh, 'syn, 'extra] list_t =
  object
    method virtual c_Nil  : 'inh -> 'syn
    method virtual c_Cons : 'inh -> 'a -> 'a list -> 'syn
  end

let gcata_list tr inh = function
| []    -> tr#c_Nil inh
| x::xs -> tr#c_Cons inh x xs

class ['a, 'extra] html_list_t fself fa =
  object
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer, 'extra] @list
    method c_Nil  _      = View.empty
    method c_Cons _ x xs =
      View.concat (fa x) (match xs with [] -> View.empty | xs -> HTML.li (fself xs))
  end

class ['a, 'extra] show_list_t fself fa =
  object
    inherit ['a, unit, string, unit, string, 'extra] @list
    method c_Nil  _      = ""
    method c_Cons _ x xs = (fa x) ^ (match xs with [] -> "" | _ -> "; " ^ fself xs)
  end

class ['a, 'sa, 'extra] gmap_list_t fself fa =
  object
    inherit ['a, unit, 'sa, unit, 'sa list, 'extra] @list
    method c_Nil  _ = []
    method c_Cons _ x xs = (fa x) :: (fself xs)
  end

class ['a, 'syn, 'extra] foldl_list_t fself fa =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn, 'extra] @list
    method c_Nil  s = s
    method c_Cons s x xs = fself  (fa s x) xs
  end

class ['a, 'syn, 'extra] foldr_list_t fself fa =
  object
    inherit ['a, 'syn, 'extra] @list[foldl] fself fa
    method c_Cons s x xs = fa (fself s xs) x
  end

class ['a, 'extra] eq_list_t fself fa =
  object
    inherit ['a, 'a, bool, 'a list, bool, 'extra] @list
    method c_Nil inh  = (inh = [])
    method c_Cons inh x xs =
      match inh with
      | y::ys -> fa y x && fself ys xs
      | _ -> false
  end

class ['a, 'extra] compare_list_t fself fa =
  object
    inherit ['a, 'a, comparison, 'a list, comparison, 'extra] @list
    method c_Nil inh =
      match inh with
      | [] -> EQ
      |  _ -> GT
    method c_Cons inh x xs =
      match inh with
      | [] -> LT
      | (y::ys) -> (match fa y x with
                   | EQ -> fself ys xs
                   | c  -> c
                   )
  end

let list : (('a, 'ia, 'sa, 'inh, 'syn, _) #list_t -> 'inh -> 'a list -> 'syn,
            < show    : ('a -> string)      -> 'a list -> string;
              html    : ('a -> HTML.viewer) -> 'a list -> HTML.viewer;
              gmap    : ('a -> 'b) -> 'a list -> 'b list;
              foldl   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              foldr   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              eq      : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool;
              compare : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison;
            >) t =
  {gcata   = gcata_list;
   plugins = object
               method show    fa l = "[" ^ (
                 fix0 (fun fself ->
                   gcata_list (new @list[show] fself fa) ()
                 )
                 l) ^ "]"
               method html    fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[html] fself fa) ()
                 )
               method gmap    fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[gmap] fself fa) ()
                 )
               method eq      fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[eq] fself fa)
                 )
               method compare fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[compare] fself fa)
                 )
               method foldl   fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[foldl] fself fa)
                 )
               method foldr   fa  =
                 fix0 (fun fself ->
                   gcata_list (new @list[foldr] fself fa)
                 )
             end
  }


module Lazy =
  struct

    type ('a, 'b) t' = ('a, 'b) t

    include Lazy

    class virtual ['a, 'ia, 'sa, 'inh, 'syn, 'extra] t_t = object
        method virtual t_t : 'inh -> 'a t -> 'syn
      end
    let gcata_t tr inh subj = tr#t_t inh subj
    let gcata_lazy = gcata_t

    class ['a, 'extra ] html_t_t _fself fa =
      object
        inherit ['a, unit, HTML.viewer, unit, HTML.viewer, 'extra ] @t
        method t_t inh subj = fa @@ Lazy.force subj
      end

    class ['a, 'extra ] show_t_t _fself fa =
      object
        inherit ['a, unit, string, unit, string, 'extra ] @t
        method t_t inh subj = fa @@ Lazy.force subj
      end

    class ['a, 'sa, 'extra ] gmap_t_t _fself fa =
      object
        inherit ['a, unit, 'sa, unit, 'sa t, 'extra ] @t
        method t_t inh subj = lazy (fa @@ Lazy.force subj)
      end

    class ['a, 'syn, 'extra ] foldl_t_t _fself fa =
      object
        inherit ['a, 'syn, 'syn, 'syn, 'syn, 'extra ] @t
        method t_t inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'syn, 'extra ] foldr_t_t fself fa =
      object
        inherit ['a, 'syn, 'extra ] @t[foldl] fself fa
      end

    class ['a, 'extra ] eq_t_t _fself fa =
      object
        inherit ['a, 'a, bool, 'a t, bool, 'extra ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end


    class ['a, 'extra ] compare_t_t fself fa =
      object
        inherit ['a, 'a, comparison, 'a t, comparison, 'extra ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    let t : ( ('a, 'ia, 'sa, 'inh, 'syn, _) #t_t -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      -> 'a t -> string;
               html    : ('a -> HTML.viewer) -> 'a t -> HTML.viewer;
               gmap    : ('a -> 'b) -> 'a t -> 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >) t' =
      let fself _ = assert false in
      {gcata   = gcata_lazy;
       plugins = object
                   method show    fa   = gcata_lazy (new @t[show] fself fa) ()
                   method html    fa   = gcata_lazy (new @t[html] fself fa) ()
                   method gmap    fa   = gcata_lazy (new @t[gmap] fself fa) ()
                   method eq      fa   = gcata_lazy (new @t[eq] fself fa)
                   method compare fa   = gcata_lazy (new @t[compare] fself fa)
                   method foldl   fa   = gcata_lazy (new @t[foldl] fself fa)
                   method foldr   fa   = gcata_lazy (new @t[foldr] fself fa)
                 end
      }
  end


type 'a poption = 'a option
type 'a option = 'a poption

let gcata_option tr inh subj =
  match subj with
  | None -> tr#c_None inh
  | Some x -> tr#c_Some inh x

class virtual ['a, 'ia, 'sa, 'inh, 'syn, 'extra] option_t =
  object
    method virtual c_None :   'inh -> 'syn
    method virtual c_Some :   'inh -> 'a  -> 'syn
  end

class ['a] html_option_t _fself fa =
  object
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer, 'extra] @option
    method c_None _  = HTML.string "None"
    method c_Some _ x = View.concat (HTML.string "Some") (HTML.ul (fa x))
  end

class ['a, 'extra] show_option_t _fself fa =
  object
    inherit ['a, unit, string, unit, string, 'extra] @option
    method c_None _  = "None"
    method c_Some _ x = "Some (" ^ fa x ^ ")"
  end

class ['a, 'sa, 'extra] gmap_option_t _fself fa =
  object
    inherit ['a, unit, 'sa, unit, 'sa option, 'extra] @option
    method c_None _ = None
    method c_Some _ x = Some (fa x)
  end

class ['a, 'syn, 'extra] foldl_option_t _fself fa =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn, 'extra] @option
    method c_None s = s
    method c_Some s x = fa s x
  end

class ['a, 'syn, 'extra] foldr_option_t _fself fa =
  object
    inherit ['a, 'syn, 'extra] @option[foldl] _fself fa
  end

class ['a, 'extra] eq_option_t _fself fa =
  object
    inherit ['a, 'a, bool, 'a option, bool, 'extra] @option
    method c_None inh = inh = None
    method c_Some inh x =
      match inh with
      | Some y -> fa y x
      | _ -> false
  end

class ['a, 'extra] compare_option_t _fself fa =
  object
    inherit ['a, 'a, comparison, 'a option, comparison, 'extra] @option
    method c_None = function
      | None -> EQ
      | _  -> GT
    method c_Some inh x =
      match inh with
      | None -> LT
      | Some y -> fa y x
  end

let option : ( ('a, 'ia, 'sa, 'inh, 'syn, _) #option_t -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
                html    : ('a -> HTML.viewer) -> 'a option -> HTML.viewer;
                gmap    : ('a -> 'b) -> 'a option -> 'b option;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool;
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison;
              >) t =
  let fself _ = assert false in (* because nonrecursive type *)
  {gcata   = gcata_option;
   plugins = object
               method show    fa = gcata_option (new @option[show] fself fa) ()
               method html    fa = gcata_option (new @option[html] fself fa) ()
               method gmap    fa = gcata_option (new @option[gmap] fself fa) ()
               method eq      fa = gcata_option (new @option[eq] fself fa)
               method compare fa = gcata_option (new @option[compare] fself fa)
               method foldl   fa = gcata_option (new @option[foldl] fself fa)
               method foldr   fa = gcata_option (new @option[foldr] fself fa)
             end
  }

(* ************************************************************************* *)
(* Antiphantom type *)
type 'a free = 'a
class virtual ['a, 'ia, 'sa, 'inh, 'syn, 'extra] free_t = object
  method virtual c_Free : 'inh -> 'a -> 'syn
end
let gcata_free tr = tr#c_Free

class ['a, 'extra] show_free_t _ fa =
  object
    inherit ['a, unit, string, unit, string, 'extra] free_t
    method c_Free () x = "(" ^ fa x ^ ")"
  end

class ['a, 'extra] html_free_t _ fa =
  object
    inherit ['a, unit, 'syn, unit, 'syn, 'extra] free_t
    constraint 'syn = HTML.viewer
    method c_Free () x = HTML.string "not implemented"
  end

class ['a, 'sa, 'extra] gmap_free_t _ fa =
  object
    inherit ['a, unit, 'sa, unit, 'sa free, 'extra] free_t
    method c_Free () x = fa x
  end

class ['a, 'syn, 'extra] foldl_free_t _ fa  =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn, 'extra] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'syn, 'extra] foldr_free_t _ fa  =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn, 'extra] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'extra] eq_free_t _ fa  =
  object
    inherit ['a, 'a, bool, 'a free, bool, 'extra] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'extra] compare_free_t _ fa  =
  object
    inherit ['a, 'a, 'syn, 'a free, 'syn, 'extra] free_t
    constraint 'syn = comparison
    method c_Free z x = fa z x
  end

let free : ( ('a, 'ia, 'sa, 'inh, 'syn, _) #free_t -> 'inh -> 'a free -> 'syn,
              < show    : ('a -> string) -> 'a free -> string;
                html    : ('a -> HTML.viewer) -> 'a free -> HTML.viewer;
                gmap    : ('a -> 'c) -> 'a free -> 'c free;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a free -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a free -> 'c;
                eq      : ('a -> 'a -> bool) ->
                          'a free -> 'a free -> bool;
                compare : ('a -> 'a -> comparison) ->
                          'a free -> 'a free -> comparison;
              >) t =
  {gcata   = gcata_free;
   plugins = object
       method show    fa = gcata_free (new show_free_t (fun _ -> assert false) fa) ()
       method html    fa = gcata_free (new html_free_t (fun _ -> assert false) fa) ()
       method gmap    fa = gcata_free (new gmap_free_t (fun _ -> assert false) fa) ()
       method eq      fa = gcata_free (new eq_free_t   (fun _ -> assert false) fa)
       method compare fa = gcata_free (new compare_free_t(fun _ -> assert false) fa)
       method foldl   fa = gcata_free (new foldl_free_t (fun _ -> assert false) fa)
       method foldr   fa = gcata_free (new foldr_free_t (fun _ -> assert false) fa)
  end
  }


(* Pairs and other stuff without explicit structure *)
type ('a, 'b) pair = 'a * 'b

let gcata_pair tr inh = function (a, b) -> tr#c_Pair inh a b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] pair_t =
  object
    method virtual c_Pair : 'inh -> 'a -> 'b -> 'syn
    (* method t_pair fa fb = transform pair fa fb this *)
  end

class ['a, 'b, 'extra] show_pair_t _ fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string, 'extra] pair_t
    method c_Pair () x y = "(" ^ fa x ^ ", " ^ fb y ^ ")"
  end

class ['a, 'b, 'extra] html_pair_t _ fa fb =
  object
    inherit ['a, unit, 'syn, 'b, unit, 'syn, unit, 'syn, 'extra] pair_t
    constraint 'syn = HTML.viewer
    method c_Pair () x y = HTML.string "not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'extra] gmap_pair_t _ (fa: 'a -> 'sa) fb =
  object
    inherit ['a, unit, 'sa, 'b, unit, 'sb, unit, ('sa, 'sb) pair, 'extra] pair_t
    method c_Pair () x y = (fa x, fb y)
  end

class ['a, 'b, 'syn, 'extra] foldl_pair_t _ fa fb  =
  object
    inherit ['a, 'syn, 'syn, 'b, 'syn, 'syn, 'syn, 'syn, 'extra] pair_t
    method c_Pair s x y = fb (fa s x) y
  end

class ['a, 'b, 'syn, 'extra] foldr_pair_t _ fa fb  =
  object
    inherit ['a, 'syn, 'syn, 'b, 'syn, 'syn, 'syn, 'syn, 'extra] pair_t
    method c_Pair s x y = fa (fb s y) x
  end

class ['a, 'b, 'extra] eq_pair_t _ fa fb  =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) pair, bool, 'extra] pair_t
    method c_Pair inh x y =
      match inh with
      (z, t) -> fa z x && fb t y
  end

class ['a, 'b, 'extra] compare_pair_t _ fa fb  =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) pair, 'syn, 'extra] pair_t
    constraint 'syn = comparison
    method c_Pair (z,t) x y = (match fa z x with EQ -> fb t y | c -> c)
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

let pair : ( ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn,_) #pair_t -> 'inh -> ('a, 'b) pair -> 'syn,
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
  {gcata   = gcata_pair;
   plugins = object
       method show    fa fb = gcata_pair (new show_pair_t (fun _ -> assert false) fa fb) ()
       method html    fa fb = gcata_pair (new html_pair_t (fun _ -> assert false) fa fb) ()
       method gmap    fa fb = gcata_pair (new gmap_pair_t (fun _ -> assert false) fa fb) ()
       method eq      fa fb = gcata_pair (new eq_pair_t   (fun _ -> assert false) fa fb)
       method compare fa fb = gcata_pair (new compare_pair_t(fun _ -> assert false) fa fb)
       method foldl   fa fb = gcata_pair (new foldl_pair_t (fun _ -> assert false) fa fb)
       method foldr   fa fb = gcata_pair (new foldr_pair_t (fun _ -> assert false) fa fb)
  end
  }

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] tuple2_t = object
  inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] pair_t
end
let gcata_tuple2 = gcata_pair
let tuple2 = pair

(* Just aliases *)
class ['a, 'b, 'extra] show_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'extra] show_pair_t fself fa fb
end
class ['a, 'a2, 'b, 'b2, 'extra] gmap_tuple2_t fself fa fb = object
  inherit [ 'a, 'a2, 'b, 'b2, 'extra] gmap_pair_t fself fa fb
end
class ['a, 'b, 'extra] compare_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'extra] compare_pair_t fself fa fb
end
class ['a, 'b, 'extra] eq_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'extra] eq_pair_t fself fa fb
end
class ['a, 'b, 'syn, 'extra] foldl_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'syn, 'extra] foldl_pair_t fself fa fb
end
class ['a, 'b, 'syn, 'extra] foldr_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'syn, 'extra] foldr_pair_t fself fa fb
end

(*******************************************************************************)
(* Tuples of size 3 *)
type ('a,'b,'c) triple = 'a * 'b * 'c
class virtual ['a,'ia,'sa, 'b,'ib,'sb, 'c,'ic,'sc, 'inh, 'syn, 'e] triple_t = object
  method virtual c_Triple : 'inh -> 'a -> 'b -> 'c -> 'syn
end
class virtual ['a,'ia,'sa, 'b,'ib,'sb, 'c,'ic,'sc, 'inh, 'syn, 'e] tuple3_t = object
  inherit     ['a,'ia,'sa, 'b,'ib,'sb, 'c,'ic,'sc, 'inh, 'syn, 'e] triple_t
end
let gcata_triple tr inh (a,b,c) = tr#c_Triple inh a b c
let gcata_tuple3 = gcata_triple

class ['a, 'b, 'c, 'extra] show_triple_t _ fa fb fc =
  object
    inherit [ 'a, unit, string
            , 'b, unit, string
            , 'c, unit, string
            , unit, string, 'extra] @triple
    method c_Triple () x y z = Printf.sprintf "(%s, %s, %s)"
      (fa x) (fb y) (fc z)
end
class ['a, 'a2, 'b, 'b2,  'c, 'c2, 'extra] gmap_triple_t _ fa fb fc =
  object
    inherit [ 'a, unit, 'a2
            , 'b, unit, 'b2
            , 'c, unit, 'c2
            , unit, ('a2,'b2,'c2) triple, 'extra ] @triple
    method c_Triple () x y z = ( (fa x), (fb y), (fc z) )
end
class ['a, 'b, 'c, 'extra] compare_triple_t _ fa fb fc =
  object
    inherit [ 'a, 'a, 'syn
            , 'b, 'b, 'syn
            , 'c, 'c, 'syn
            , 'inh, 'syn, 'extra  ] @triple
    constraint 'inh = ('a, 'b, 'c) triple
    constraint 'syn = comparison
    method c_Triple (a,b,c) x y z =
      chain_compare (fa a x) @@ fun _ ->
      chain_compare (fb b y) @@ fun _ ->
       (fc c z)
end

class ['a, 'b, 'c, 'extra] eq_triple_t _ fa fb fc =
  object
    inherit [ 'a, 'a, bool
            , 'b, 'b, bool
            , 'c, 'c, bool
            , ('a, 'b, 'c) triple, bool, 'extra] @triple
    method c_Triple inh x y z =
      match inh with
      (a, b, c) -> fa a x && fb b y && fc c z
  end

class ['a, 'b, 'c, 'syn, 'extra] foldl_triple_t _ fa fb fc =
  object
    inherit [ 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'c, 'syn, 'syn
            , 'syn, 'syn, 'extra] @triple
    method c_Triple s x y z = fc (fb (fa s x) y) z
  end

class ['a, 'b, 'c, 'syn, 'extra] foldr_triple_t _ fa fb fc =
  object
    inherit ['a, 'syn, 'syn, 'b, 'syn, 'syn, 'c, 'syn, 'syn
            , 'syn, 'syn, 'extra] triple_t
    method c_Triple s x y z = fa (fb (fc s z) y) x
  end

let triple :
    ( ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'c, 'ic, 'sc, 'inh, 'syn, 'extra ) #triple_t ->
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
                  comparison;
        eq      : ('a -> 'a -> bool) ->
                  ('b -> 'b -> bool) ->
                  ('c -> 'c -> bool) ->
                  ('a, 'b, 'c) triple ->
                  ('a, 'b, 'c) triple ->
                  bool;
        foldl   : ('syn -> 'a -> 'syn) ->
                  ('syn -> 'b -> 'syn) ->
                  ('syn -> 'c -> 'syn) ->
                  'syn ->
                  ('a, 'b, 'c) triple ->
                  'syn;
      >) t =
  {gcata   = gcata_triple;
   plugins = object
     method show    fa fb fc =
       gcata_triple (new show_triple_t    (fun _ -> assert false) fa fb fc) ()
     method gmap    fa fb fc =
       gcata_triple (new gmap_triple_t    (fun _ -> assert false) fa fb fc) ()
     method compare fa fb fc inh =
       gcata_triple (new compare_triple_t (fun _ -> assert false) fa fb fc) inh
     method eq      fa fb fc inh =
       gcata_triple (new eq_triple_t (fun _ -> assert false) fa fb fc) inh
     method foldl fa fb fc inh =
       gcata_triple (new foldl_triple_t (fun _ -> assert false) fa fb fc) inh
  end
}

let tuple3 = triple

class ['a, 'b, 'c, 'extra] show_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'extra] show_triple_t fself fa fb fc
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'extra] gmap_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'extra] gmap_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'extra] compare_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'extra] compare_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'extra] eq_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'extra] eq_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'syn, 'extra] foldl_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'syn, 'extra] foldl_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'syn, 'extra] foldr_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'syn, 'extra] foldr_triple_t fself fa fb fc
end

(****************************************************************************)
let show    t = t.plugins#show
let html    t = t.plugins#html
let gmap    t = t.plugins#gmap
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare


