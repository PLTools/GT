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

let string_of_string  s = s
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

type 'a plist = 'a list
type 'a list = 'a plist

class type html_list_env_tt = object  end
class type show_list_env_tt = object  end
class type foldl_list_env_tt = object  end
class type foldr_list_env_tt = object  end
class type eq_list_env_tt = object  end
class type compare_list_env_tt = object  end
class type map_list_env_tt = object  end

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
    inherit ['a, unit, HTMLView.viewer, unit, HTMLView.viewer] @list
    method c_Nil  _ _      = View.empty
    method c_Cons _ _ x xs = View.concat (x.fx ()) (match xs.x with [] -> View.empty | _ -> HTMLView.li (xs.fx ()))
  end

class ['a] show_list_t =
  object
    inherit ['a, unit, string, unit, string] @list
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = x.fx () ^ (match xs.x with [] -> "" | _ -> ", " ^ xs.fx ())
  end
      
class ['a, 'sa] map_list_t =
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
              html    : ('a -> HTMLView.er) -> 'a list -> HTMLView.er;
              map     : ('a -> 'b) -> 'a list -> 'b list; 
              foldl   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c; 
              foldr   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;             
              eq      : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool; 
              compare : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison; 
            >) t =
  {gcata   = list.gcata; 
   plugins = object
               method show    fa = transform(list) (lift fa) (new @list[show]) ()
               method html    fa = transform(list) (lift fa) (new @list[html]) ()
               method map     fa = transform(list) (lift fa) (new @list[map] ) ()
               method eq      fa = transform(list) fa (new @list[eq]) 
               method compare fa = transform(list) fa (new @list[compare])
               method foldl   fa = transform(list) fa (new @list[foldl])
               method foldr   fa = transform(list) fa (new @list[foldr])
             end
  }

type 'a poption = 'a option
type 'a option = 'a poption

class type html_option_env_tt = object  end
class type show_option_env_tt = object  end
class type foldl_option_env_tt = object  end
class type foldr_option_env_tt = object  end
class type eq_option_env_tt = object  end
class type compare_option_env_tt = object  end
class type map_option_env_tt = object  end

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
    inherit ['a, unit, HTMLView.viewer, unit, HTMLView.viewer] @option
    method c_None  _ _  = HTMLView.string "None"
    method c_Some _ _ x = View.concat (HTMLView.string "Some") (HTMLView.ul (x.fx ()))
  end

class ['a] show_option_t =
  object
    inherit ['a, unit, string, unit, string] @option
    method c_None  _ _  = "None"
    method c_Some _ _ x = "Some (" ^ x.fx () ^ ")"
  end
      
class ['a, 'sa] map_option_t =
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
                html    : ('a -> HTMLView.er) -> 'a option -> HTMLView.er;
                map     : ('a -> 'b) -> 'a option -> 'b option; 
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c; 
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;             
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool; 
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison; 
              >) t =
  {gcata   = option.gcata; 
   plugins = object
               method show    fa = transform(option) (lift fa) (new @option[show]) ()
               method html    fa = transform(option) (lift fa) (new @option[html]) ()
               method map     fa = transform(option) (lift fa) (new @option[map] ) ()
               method eq      fa = transform(option) fa (new @option[eq]) 
               method compare fa = transform(option) fa (new @option[compare])
               method foldl   fa = transform(option) fa (new @option[foldl])
               method foldr   fa = transform(option) fa (new @option[foldr])
             end
  }

let show    t = t.plugins#show
let html    t = t.plugins#html
let map     t = t.plugins#map
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare


