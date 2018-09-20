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

module Format = struct
  include Format
  let pp_print_unit  fmt () = pp_print_string fmt "()"
  let pp_print_int32 fmt n  = Format.pp_print_string fmt @@ Int32.format "%d" n
  let pp_print_int64 fmt n  = Format.pp_print_string fmt @@ Int64.format "%d" n
  let pp_print_nativeint fmt n = Format.pp_print_string fmt @@ Nativeint.format "%d" n
  let pp_print_string fmt s = fprintf fmt "\"%s\"" s
end

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
GENERIFY(float)
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

(* ************************************************************************* *)
(** Standart types go there *)
type 'a plist      = 'a list
type 'a list       = 'a plist

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] list_t =
  object
    method virtual c_Nil  : 'inh -> 'syn
    method virtual c_Cons : 'inh -> 'a -> 'a list -> 'syn
  end

let gcata_list tr inh = function
| []    -> tr#c_Nil inh
| x::xs -> tr#c_Cons inh x xs

class ['a, 'self] html_list_t fself fa =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @list
    method c_Nil  _      = View.empty
    method c_Cons _ x xs =

      HTML.ul @@ HTML.seq (
        [ HTML.string "list" ] @ List.map (fun x -> HTML.li @@ fa x) (x::xs)
        )
(*      View.concat (fa x) (match xs with [] -> View.empty | xs -> HTML.li (fself xs)) *)
  end

class ['a, 'self] show_list_t fself fa =
  object
    inherit [unit, 'a, string, unit, 'self, string] @list
    method c_Nil  _      = ""
    method c_Cons _ x xs = (fa x) ^ (match xs with [] -> "" | _ -> "; " ^ fself xs)
  end

class ['a, 'self] fmt_list_t fself fa =
  object
    inherit ['inh, 'a, unit, 'inh, 'self, unit] @list
    constraint 'inh = Format.formatter
    method c_Nil  _      = ()
    method c_Cons fmt x xs =
      Format.fprintf fmt "%a;@,@ %a" fa x fself xs
  end

class ['a, 'sa, 'self] gmap_list_t fself fa =
  object
    inherit [unit, 'a, 'sa, unit, 'self, 'sa list] @list
    method c_Nil  _ = []
    method c_Cons _ x xs = (fa x) :: (fself xs)
  end
class ['a, 'sa, 'env, 'self] eval_list_t fself fa =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'sa list] @list
    method c_Nil  _ = []
    method c_Cons env x xs = (fa env x) :: (fself env xs)
  end
class ['a, 'sa, 'env, 'self] stateful_list_t fself fa =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa list] @list
    method c_Nil  env = (env, [])
    method c_Cons env0 x xs : 'env * 'sa list =
      let env1,h  = fa    env0 x  in
      let env2,tl = fself env1 xs in
      env2, (h::tl)
  end

class ['a, 'syn, 'self] foldl_list_t fself fa =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @list
    method c_Nil  s = s
    method c_Cons s x xs = fself  (fa s x) xs
  end

class ['a, 'syn, 'self] foldr_list_t fself fa =
  object
    inherit ['a, 'syn, 'self] @list[foldl] fself fa
    method c_Cons s x xs = fa (fself s xs) x
  end

class ['a, 'self] eq_list_t fself fa =
  object
    inherit ['a, 'a, bool, 'a list, 'self, bool] @list
    method c_Nil inh  = (inh = [])
    method c_Cons inh x xs =
      match inh with
      | y::ys -> fa y x && fself ys xs
      | _ -> false
  end

class ['a, 'self] compare_list_t fself fa =
  object
    inherit ['a, 'a, comparison, 'a list, 'self, comparison] @list
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

let list : (('ia, 'a, 'sa, 'inh, _, 'syn) #list_t -> 'inh -> 'a list -> 'syn,
            < show    : ('a -> string)      -> 'a list -> string;
              fmt     : (Format.formatter -> 'a -> unit) ->
                        Format.formatter -> 'a list -> unit;
              html    : ('a -> HTML.viewer) -> 'a list -> HTML.viewer;
              gmap    : ('a -> 'b) -> 'a list -> 'b list;
              eval    : ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list;
              stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a list -> 'env * 'b list;
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
               method fmt fa inh l =
                 Format.fprintf inh "[@[%a@]]@,"
                   (fix0 (fun fself ->
                     gcata_list (new @list[fmt] fself fa)
                   ))
                   l


               method html    fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[html] fself fa) ()
                 )
               method gmap    fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[gmap] fself fa) ()
                 )
               method stateful fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[stateful] fself fa)
                 )
               method eval    fa   =
                 fix0 (fun fself ->
                   gcata_list (new @list[eval] fself fa)
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

    class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] t_t = object
        method virtual t_t : 'inh -> 'a t -> 'syn
      end
    let gcata_t tr inh subj = tr#t_t inh subj
    let gcata_lazy = gcata_t

    class ['a, 'self ] html_t_t _fself fa =
      object
        inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer ] @t
        method t_t inh subj = fa @@ Lazy.force subj
      end

    class ['a, 'self ] show_t_t _fself fa =
      object
        inherit [unit, 'a, string, unit, 'self, string ] @t
        method t_t inh subj = fa @@ Lazy.force subj
      end

    class ['a, 'sa, 'self ] gmap_t_t _fself fa =
      object
        inherit [unit, 'a, 'sa, unit, 'self, 'sa t ] @t
        method t_t inh subj = lazy (fa @@ Lazy.force subj)
      end

    class ['a, 'sa, 'env, 'self ] eval_t_t _fself fa =
      object
        inherit ['env, 'a, 'sa, 'env, 'self, 'sa t ] @t
        method t_t env subj = lazy (fa env @@ Lazy.force subj)
      end

    class ['a, 'sa, 'env, 'self ] stateful_t_t _fself fa =
      object
        inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'sa t ] @t
        method t_t env subj =
          let (env1, r) = fa env @@ Lazy.force subj
          in env1, Lazy.from_fun (fun () -> r)
          (* THE SAME AS eval *)
      end

    class ['a, 'syn, 'self ] foldl_t_t _fself fa =
      object
        inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn ] @t
        method t_t inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'syn, 'self ] foldr_t_t fself fa =
      object
        inherit ['a, 'syn, 'self ] @t[foldl] fself fa
      end

    class ['a, 'self ] eq_t_t _fself fa =
      object
        inherit ['a, 'a, bool, 'a t, 'self, bool ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    class ['a, 'self ] compare_t_t fself fa =
      object
        inherit ['a, 'a, comparison, 'a t, 'self, comparison ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    let t : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #t_t -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      -> 'a t -> string;
               html    : ('a -> HTML.viewer) -> 'a t -> HTML.viewer;
               gmap    : ('a -> 'b) -> 'a t -> 'b t;
               eval    : ('env -> 'a -> 'b) -> 'env -> 'a t -> 'b t;
               stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a t -> 'env * 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >) t' =
      let fself _ = assert false in
      {gcata   = gcata_lazy;
       plugins = object
                   method show     fa  = gcata_lazy (new @t[show] fself fa) ()
                   method html     fa  = gcata_lazy (new @t[html] fself fa) ()
                   method gmap     fa  = gcata_lazy (new @t[gmap] fself fa) ()
                   method eval     fa  = gcata_lazy (new @t[eval] fself fa)
                   method stateful fa  = gcata_lazy (new @t[stateful] fself fa)
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

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] option_t =
  object
    method virtual c_None :   'inh -> 'syn
    method virtual c_Some :   'inh -> 'a  -> 'syn
  end

class ['a, 'self] show_option_t _fself fa =
  object
    inherit [ unit, 'a, string, unit, 'self, string] @option
    method c_None _  = "None"
    method c_Some _ x = "Some (" ^ fa x ^ ")"
  end
class ['a, 'self] fmt_option_t _fself fa =
  object
    inherit [ Format.formatter, 'a, unit, Format.formatter, 'self, unit] @option
    method c_None fmt   = Format.fprintf fmt "None"
    method c_Some fmt x = Format.fprintf fmt "Some (%a)" fa x
  end
class ['a, 'self] html_option_t _fself fa =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @option
    method c_None _  = HTML.string "None"
    method c_Some _ x = View.concat (HTML.string "Some") (HTML.ul (fa x))
  end


class ['a, 'sa, 'self] gmap_option_t _fself fa =
  object
    inherit [unit, 'a, 'sa, unit, 'self, 'sa option] @option
    method c_None _ = None
    method c_Some _ x = Some (fa x)
  end

class ['a, 'sa, 'env, 'self] eval_option_t _fself fa =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'sa option] @option
    method c_None _ = None
    method c_Some env x = Some (fa env x)
  end

class ['a, 'sa, 'env, 'self] stateful_option_t _fself fa =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'sa option] @option
    method c_None env = (env,None)
    method c_Some env x =
      let env1,r = fa env x in
      (env1, Some r)
  end

class ['a, 'syn, 'self] foldl_option_t _fself fa =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @option
    method c_None s = s
    method c_Some s x = fa s x
  end

class ['a, 'syn, 'self] foldr_option_t _fself fa =
  object
    inherit ['a, 'syn, 'self] @option[foldl] _fself fa
  end

class ['a, 'self] eq_option_t _fself fa =
  object
    inherit ['a, 'a, bool, 'a option, 'self, bool] @option
    method c_None inh = inh = None
    method c_Some inh x =
      match inh with
      | Some y -> fa y x
      | _ -> false
  end

class ['a, 'self] compare_option_t _fself fa =
  object
    inherit ['a, 'a, comparison, 'a option, 'self, comparison] @option
    method c_None = function
      | None -> EQ
      | _  -> GT
    method c_Some inh x =
      match inh with
      | None -> LT
      | Some y -> fa y x
  end

let option : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #option_t -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
                fmt     : (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a option -> unit;
                html    : ('a -> HTML.viewer) -> 'a option -> HTML.viewer;
                gmap    : ('a -> 'b) -> 'a option -> 'b option;
                stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a option -> 'env * 'b option;
                eval    : ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool;
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison;
              >) t =
  let fself _ = assert false in (* because nonrecursive type *)
  {gcata   = gcata_option;
   plugins = object
               method show     fa = gcata_option (new @option[show] fself fa) ()
               method fmt      fa = gcata_option (new @option[fmt] fself fa)
               method html     fa = gcata_option (new @option[html] fself fa) ()
               method gmap     fa = gcata_option (new @option[gmap] fself fa) ()
               method stateful fa = gcata_option (new @option[stateful] fself fa)
               method eval    fa = gcata_option (new @option[eval] fself fa)
               method eq      fa = gcata_option (new @option[eq] fself fa)
               method compare fa = gcata_option (new @option[compare] fself fa)
               method foldl   fa = gcata_option (new @option[foldl] fself fa)
               method foldr   fa = gcata_option (new @option[foldr] fself fa)
             end
  }

(* ************************************************************************* *)
(* Antiphantom type *)
type 'a free = 'a
class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] free_t = object
  method virtual c_Free : 'inh -> 'a -> 'syn
end
let gcata_free tr = tr#c_Free

class ['a, 'self] show_free_t _ fa =
  object
    inherit [unit, 'a, string, unit, 'self, string] free_t
    method c_Free () x = "(" ^ fa x ^ ")"
  end
class ['a, 'self] fmt_free_t _ fa =
  object
    inherit ['inh, 'a, unit, 'inh, 'self, unit] free_t
    constraint 'inh = Format.formatter
    method c_Free fmt x = Format.fprintf fmt "(%a)" fa x
  end

class ['a, 'self] html_free_t _ fa =
  object
    inherit [unit, 'a, 'syn, unit, 'self, 'syn] free_t
    constraint 'syn = HTML.viewer
    method c_Free () x = fa x
  end

class ['a, 'sa, 'self] gmap_free_t _ fa =
  object
    inherit [unit, 'a, 'sa, unit, 'self, 'sa free] free_t
    method c_Free () x = fa x
  end
class ['a, 'sa, 'env, 'self] eval_free_t _ fa =
  object
    inherit ['emv, 'a, 'sa, 'env, 'self, 'sa free] free_t
    method c_Free env x = fa env x
  end

class ['a, 'sa, 'env, 'self] stateful_free_t _ fa =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa free] free_t
    method c_Free env x = fa env x
  end

class ['a, 'syn, 'self] foldl_free_t _ fa  =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'syn, 'self] foldr_free_t _ fa  =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'self] eq_free_t _ fa  =
  object
    inherit ['a, 'a, bool, 'a free, 'self, bool] free_t
    method c_Free inh x = fa inh x
  end

class ['a, 'self] compare_free_t _ fa  =
  object
    inherit ['a, 'a, 'syn, 'a free, 'self, 'syn] free_t
    constraint 'syn = comparison
    method c_Free z x = fa z x
  end

let free : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #free_t -> 'inh -> 'a free -> 'syn,
              < show    : ('a -> string) -> 'a free -> string;
                fmt     : (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a free -> unit;
                html    : ('a -> HTML.viewer) -> 'a free -> HTML.viewer;
                gmap    : ('a -> 'c) -> 'a free -> 'c free;
                eval    : ('env -> 'a -> 'c) -> 'env -> 'a free -> 'c free;
                stateful: ('env -> 'a -> 'env * 'c) -> 'env -> 'a free -> 'env * 'c free;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a free -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a free -> 'c;
                eq      : ('a -> 'a -> bool) ->
                          'a free -> 'a free -> bool;
                compare : ('a -> 'a -> comparison) ->
                          'a free -> 'a free -> comparison;
              >) t =
  {gcata   = gcata_free;
   plugins = object
       method show     fa = gcata_free (new show_free_t (fun _ -> assert false) fa) ()
       method fmt      fa = gcata_free (new @free[fmt]  (fun _ -> assert false) fa)
       method html     fa = gcata_free (new html_free_t (fun _ -> assert false) fa) ()
       method gmap     fa = gcata_free (new gmap_free_t (fun _ -> assert false) fa) ()
       method eval     fa = gcata_free (new @free[eval] (fun _ _ -> assert false) fa)
       method stateful fa = gcata_free (new @free[stateful] (fun _ _ -> assert false) fa)
       method eq       fa = gcata_free (new eq_free_t   (fun _ -> assert false) fa)
       method compare  fa = gcata_free (new compare_free_t(fun _ -> assert false) fa)
       method foldl    fa = gcata_free (new foldl_free_t (fun _ -> assert false) fa)
       method foldr    fa = gcata_free (new foldr_free_t (fun _ -> assert false) fa)
  end
  }


(* Pairs and other stuff without explicit structure *)
type ('a, 'b) pair = 'a * 'b

let gcata_pair tr inh = function (a, b) -> tr#c_Pair inh a b

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] pair_t =
  object
    method virtual c_Pair : 'inh -> 'a -> 'b -> 'syn
    (* method t_pair fa fb = transform pair fa fb this *)
  end

class ['a, 'b, 'self] show_pair_t _ fa fb =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] pair_t
    method c_Pair () x y = "(" ^ fa x ^ ", " ^ fb y ^ ")"
  end
class ['a, 'b, 'self] fmt_pair_t _ fa fb =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'self, unit] pair_t
    constraint 'inh = Format.formatter
    method c_Pair fmt x y = Format.fprintf fmt "(%a,%a)" fa x fb y
  end

class ['a, 'b, 'self] html_pair_t _ fa fb =
  object
    inherit [unit, 'a, 'syn, unit, 'b, 'syn, unit, 'self, 'syn] pair_t
    constraint 'syn = HTML.viewer
    method c_Pair () x y =
      List.fold_left View.concat View.empty
         [ HTML.ul (fa x)
         ; HTML.ul (fb y)
         ]
  end

class ['a, 'sa, 'b, 'sb, 'self] gmap_pair_t _ (fa: 'a -> 'sa) fb =
  object
    inherit [unit, 'a, 'sa, unit, 'b, 'sb, unit, 'self, ('sa, 'sb) pair] pair_t
    method c_Pair () x y = (fa x, fb y)
  end

class ['a, 'sa, 'b, 'sb, 'env, 'self] eval_pair_t _ fa fb =
  object
    inherit ['env, 'a, 'sa, 'env, 'b, 'sb, 'env, 'self, ('sa, 'sb) pair] pair_t
    method c_Pair env x y = (fa env x, fb env y)
  end
class ['a, 'sa, 'b, 'sb, 'env, 'self] stateful_pair_t _ fa fb =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'b, 'sb, 'env, 'self, 'env * ('sa, 'sb) pair] pair_t
    method c_Pair env x y =
      let env1,l = fa env x in
      let env2,r = fb env y in
      env, (l,r)
  end

class ['a, 'b, 'syn, 'self] foldl_pair_t _ fa fb  =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] pair_t
    method c_Pair s x y = fb (fa s x) y
  end

class ['a, 'b, 'syn, 'self] foldr_pair_t _ fa fb  =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] pair_t
    method c_Pair s x y = fa (fb s y) x
  end

class ['a, 'b, 'self] eq_pair_t _ fa fb  =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) pair, 'self, bool] pair_t
    method c_Pair inh x y =
      match inh with
      (z, t) -> fa z x && fb t y
  end

class ['a, 'b, 'self] compare_pair_t _ fa fb  =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) pair, 'self, 'syn] pair_t
    constraint 'syn = comparison
    method c_Pair (z,t) x y = (match fa z x with EQ -> fb t y | c -> c)
  end

let pair:
  ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, _, 'syn) #pair_t -> 'inh -> ('a, 'b) pair -> 'syn,
              < show    : ('a -> string) -> ('b -> string) -> ('a, 'b) pair -> string;
                fmt     : (Format.formatter -> 'a -> unit) ->
                          (Format.formatter -> 'b -> unit) ->
                          Format.formatter -> ('a,'b) pair -> unit;
                html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) ->
                          ('a, 'b) pair -> HTML.viewer;
                gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) pair -> ('c, 'd) pair;
                stateful: ('env -> 'a -> 'env * 'c) ->
                          ('env -> 'b -> 'env * 'd) ->
                          'env -> ('a, 'b) pair -> 'env * ('c, 'd) pair;
                eval    : ('env -> 'a -> 'c) -> ('env -> 'b -> 'd) ->
                          'env -> ('a, 'b) pair -> ('c, 'd) pair;
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
       method fmt     fa fb = gcata_pair (new @pair[fmt]  (fun _ -> assert false) fa fb)
       method html    fa fb = gcata_pair (new html_pair_t (fun _ -> assert false) fa fb) ()
       method gmap    fa fb = gcata_pair (new gmap_pair_t (fun _ -> assert false) fa fb) ()
       method eval    fa fb = gcata_pair (new @pair[eval] (fun _ -> assert false) fa fb)
       method stateful fa fb= gcata_pair (new @pair[stateful] (fun _ _ -> assert false) fa fb)
       method eq      fa fb = gcata_pair (new eq_pair_t   (fun _ -> assert false) fa fb)
       method compare fa fb = gcata_pair (new compare_pair_t(fun _ -> assert false) fa fb)
       method foldl   fa fb = gcata_pair (new foldl_pair_t (fun _ -> assert false) fa fb)
       method foldr   fa fb = gcata_pair (new foldr_pair_t (fun _ -> assert false) fa fb)
  end
  }

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] tuple2_t = object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] pair_t
end
let gcata_tuple2 = gcata_pair
let tuple2 = pair

(* Just aliases *)
class ['a, 'b, 'self] show_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'self] show_pair_t fself fa fb
end
class ['a, 'b, 'self] fmt_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'self] fmt_pair_t fself fa fb
end
class ['a, 'b, 'self] html_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'self] html_pair_t fself fa fb
end
class ['a, 'a2, 'b, 'b2, 'self] gmap_tuple2_t fself fa fb = object
  inherit [ 'a, 'a2, 'b, 'b2, 'self] gmap_pair_t fself fa fb
end
class ['a, 'a2, 'b, 'b2, 'env, 'self] eval_tuple2_t fself fa fb = object
  inherit [ 'a, 'a2, 'b, 'b2, 'env, 'self] eval_pair_t fself fa fb
end
class ['a, 'a2, 'b, 'b2, 'env, 'self] stateful_tuple2_t fself fa fb = object
  inherit [ 'a, 'a2, 'b, 'b2, 'env, 'self] stateful_pair_t fself fa fb
end
class ['a, 'b, 'self] compare_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'self] compare_pair_t fself fa fb
end
class ['a, 'b, 'self] eq_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'self] eq_pair_t fself fa fb
end
class ['a, 'b, 'syn, 'self] foldl_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'syn, 'self] foldl_pair_t fself fa fb
end
class ['a, 'b, 'syn, 'self] foldr_tuple2_t fself fa fb = object
  inherit [ 'a, 'b, 'syn, 'self] foldr_pair_t fself fa fb
end

(*******************************************************************************)
(* Tuples of size 3 *)
type ('a,'b,'c) triple = 'a * 'b * 'c
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] triple_t = object
  method virtual c_Triple : 'inh -> 'a -> 'b -> 'c -> 'syn
end
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] tuple3_t = object
  inherit     ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] triple_t
end
let gcata_triple tr inh (a,b,c) = tr#c_Triple inh a b c
let gcata_tuple3 = gcata_triple

class ['a, 'b, 'c, 'self] show_triple_t _ fa fb fc =
  object
    inherit [ unit, 'a, string
            , unit, 'b, string
            , unit, 'c, string
            , unit, 'self, string] @triple
    method c_Triple () x y z = Printf.sprintf "(%s, %s, %s)"
      (fa x) (fb y) (fc z)
end
class ['a, 'b, 'c, 'self] fmt_triple_t _ fa fb fc =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'c, unit, 'inh, 'self, unit] triple_t
    constraint 'inh = Format.formatter
    method c_Triple fmt x y z = Format.fprintf fmt "(%a,%a,%a)" fa x fb y fc z
  end

class ['a, 'a2, 'b, 'b2,  'c, 'c2, 'self] gmap_triple_t _ fa fb fc =
  object
    inherit [ unit, 'a, 'a2
            , unit, 'b, 'b2
            , unit, 'c, 'c2
            , unit, 'self, ('a2,'b2,'c2) triple ] @triple
    method c_Triple () x y z = ( (fa x), (fb y), (fc z) )
end
class ['a, 'b, 'c, 'self] html_triple_t _ fa fb fc =
  object
    inherit [ unit, 'a, 'syn, unit, 'b, 'syn, unit, 'c, 'syn
            , unit, 'self, 'syn] triple_t
    constraint 'syn = HTML.viewer
    method c_Triple () x y z =
      List.fold_left View.concat View.empty
         [ HTML.string "("
         ; HTML.ul (fa x)
         ; HTML.string ", "
         ; HTML.ul (fb y)
         ; HTML.string ", "
         ; HTML.ul (fc z)
         ; HTML.string ")"]
  end

class ['a, 'a2, 'b, 'b2,  'c, 'c2, 'env, 'self] eval_triple_t _ fa fb fc =
  object
    inherit [ 'env, 'a, 'a2
            , 'env, 'b, 'b2
            , 'env, 'c, 'c2
            , 'env, 'self, ('a2,'b2,'c2) triple ] @triple
    method c_Triple e x y z = ( (fa e x), (fb e y), (fc e z) )
end
class ['a, 'a2, 'b, 'b2,  'c, 'c2, 'env, 'self] stateful_triple_t _ fa fb fc =
  object
    inherit [ 'env, 'a, 'env * 'a2
            , 'env, 'b, 'env * 'b2
            , 'env, 'c, 'env * 'c2
            , 'env, 'self, 'env * ('a2,'b2,'c2) triple ] @triple
    method c_Triple env0 x y z =
      let env1,a = fa env0 x in
      let env2,b = fb env1 y in
      let env3,c = fc env2 z in
      env3, (a,b,c)
end
class ['a, 'b, 'c, 'self] compare_triple_t _ fa fb fc =
  object
    inherit [ 'a, 'a, 'syn
            , 'b, 'b, 'syn
            , 'c, 'c, 'syn
            , 'inh, 'self, 'syn ] @triple
    constraint 'inh = ('a, 'b, 'c) triple
    constraint 'syn = comparison
    method c_Triple (a,b,c) x y z =
      chain_compare (fa a x) @@ fun _ ->
      chain_compare (fb b y) @@ fun _ ->
       (fc c z)
end

class ['a, 'b, 'c, 'self] eq_triple_t _ fa fb fc =
  object
    inherit [ 'a, 'a, bool
            , 'b, 'b, bool
            , 'c, 'c, bool
            , ('a, 'b, 'c) triple, 'self, bool] @triple
    method c_Triple inh x y z =
      match inh with
      (a, b, c) -> fa a x && fb b y && fc c z
  end

class ['a, 'b, 'c, 'syn, 'self] foldl_triple_t _ fa fb fc =
  object
    inherit [ 'syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'c, 'syn
            , 'syn, 'self, 'syn] @triple
    method c_Triple s x y z = fc (fb (fa s x) y) z
  end

class ['a, 'b, 'c, 'syn, 'self] foldr_triple_t _ fa fb fc =
  object
    inherit [ 'syn, 'a, 'syn
            , 'syn, 'b, 'syn
            , 'syn, 'c, 'syn
            , 'syn, 'self, 'syn] triple_t
    method c_Triple s x y z = fa (fb (fc s z) y) x
  end

let triple :
    ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'ic, 'c, 'sc, 'inh, _, 'syn ) #triple_t ->
      'inh -> ('a, 'b, 'c) triple -> 'syn
    , < show    : ('a -> string) -> ('b -> string) ->  ('c -> string) ->
                  ('a, 'b, 'c) triple  -> string;
        fmt     : (Format.formatter -> 'a -> unit) ->
                  (Format.formatter -> 'b -> unit) ->
                  (Format.formatter -> 'c -> unit) ->
                  Format.formatter -> ('a, 'b, 'c) triple -> unit;
        gmap    : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) ->
                  ('a, 'b, 'c) triple -> ('d, 'e, 'f) triple;
        html    : ('a -> HTML.er) -> ('b -> HTML.er) -> ('c -> HTML.er) ->
                  ('a, 'b, 'c) triple -> HTML.er;
        eval    : ('env -> 'a -> 'd) -> ('env -> 'b -> 'e) -> ('env -> 'c -> 'f) ->
                  'env -> ('a, 'b, 'c) triple -> ('d, 'e, 'f) triple;
        stateful: ('env -> 'a -> 'env * 'd) ->
                  ('env -> 'b -> 'env * 'e) ->
                  ('env -> 'c -> 'env * 'f) ->
                  'env -> ('a, 'b, 'c) triple -> 'env * ('d, 'e, 'f) triple;
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
       gcata_triple (new @triple[show]    (fun _ -> assert false) fa fb fc) ()
     method fmt     fa fb fc =
       gcata_triple (new @triple[fmt]     (fun _ -> assert false) fa fb fc)
     method gmap    fa fb fc =
       gcata_triple (new @triple[gmap]    (fun _ -> assert false) fa fb fc) ()
     method html    fa fb fc =
       gcata_triple (new @triple[html]    (fun _ -> assert false) fa fb fc) ()
     method eval    fa fb fc =
       gcata_triple (new @triple[eval]    (fun _ -> assert false) fa fb fc)
     method stateful    fa fb fc =
       gcata_triple (new @triple[stateful] (fun _ _ -> assert false) fa fb fc)
     method compare fa fb fc inh =
       gcata_triple (new compare_triple_t (fun _ -> assert false) fa fb fc) inh
     method eq      fa fb fc inh =
       gcata_triple (new eq_triple_t (fun _ -> assert false) fa fb fc) inh
     method foldl fa fb fc inh =
       gcata_triple (new foldl_triple_t (fun _ -> assert false) fa fb fc) inh
  end
}

let tuple3 = triple

class ['a, 'b, 'c, 'self] show_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'self] show_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'self] fmt_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'self] @triple[fmt] fself fa fb fc
end
class ['a, 'b, 'c, 'self] html_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'self] @triple[html] fself fa fb fc
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self] gmap_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'self] gmap_triple_t fself fa fb fc
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] eval_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] eval_triple_t fself fa fb fc
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] stateful_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] stateful_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'self] compare_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'self] compare_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'self] eq_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'self] eq_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'syn, 'self] foldl_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] foldl_triple_t fself fa fb fc
end
class ['a, 'b, 'c, 'syn, 'self] foldr_tuple3_t fself fa fb fc = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] foldr_triple_t fself fa fb fc
end

(*******************************************************************************)
(* Tuples of size 3 *)
type ('a, 'b, 'c, 'd) tuple4 = 'a * 'b * 'c * 'd
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'id,'d,'sd, 'inh, 'e, 'syn] tuple4_t =
object
  method virtual c_tuple4 : 'inh -> 'a -> 'b -> 'c -> 'd -> 'syn
end
let gcata_tuple4 tr inh (a,b,c,d) = tr#c_tuple4 inh a b c d

class ['a, 'b, 'c, 'd, 'self] fmt_tuple4_t _ fa fb fc fd =
  object
    inherit [ 'inh, 'a, unit
            , 'inh, 'b, unit
            , 'inh, 'c, unit
            , 'inh, 'd, unit
            , 'inh, 'self, unit] tuple4_t
    constraint 'inh = Format.formatter
    method c_tuple4 fmt a b c d =
      Format.fprintf fmt "(%a,%a,%a,%a)" fa a fb b fc c fd d
  end
class ['a, 'b, 'c, 'd, 'self] html_tuple4_t _ fa fb fc fd =
  object
    inherit [ unit, 'a, 'syn
            , unit, 'b, 'syn
            , unit, 'c, 'syn
            , unit, 'd, 'syn
            , unit, 'self, 'syn] tuple4_t
    constraint 'syn = HTML.viewer
    method c_tuple4 () x y z d =
      List.fold_left View.concat View.empty
         [ HTML.string "("
         ; HTML.ul (fa x)
         ; HTML.string ", "
         ; HTML.ul (fb y)
         ; HTML.string ", "
         ; HTML.ul (fc z)
         ; HTML.string ")"
         ; HTML.ul (fd d)
         ; HTML.string ")"
         ]
  end

let tuple4 :
    ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'ic, 'c, 'sc, 'id, 'd, 'sd, 'inh, _, 'syn ) #tuple4_t ->
      'inh -> ('a, 'b, 'c, 'd) tuple4 -> 'syn
    , < fmt     : (Format.formatter -> 'a -> unit) ->
                  (Format.formatter -> 'b -> unit) ->
                  (Format.formatter -> 'c -> unit) ->
                  (Format.formatter -> 'd -> unit) ->
                  Format.formatter -> ('a, 'b, 'c, 'd) tuple4 -> unit;
        html    : ('a -> HTML.er) -> ('b -> HTML.er) -> ('c -> HTML.er) ->
                  ('d -> HTML.er) ->
                  ('a, 'b, 'c, 'd) tuple4 -> HTML.er;
      >) t =
  {gcata   = gcata_tuple4;
   plugins = object
     method fmt     fa fb fc fd =
       gcata_tuple4 (new @tuple4[fmt]     (fun _ -> assert false) fa fb fc fd)
     method html    fa fb fc fd =
       gcata_tuple4 (new @tuple4[html]    (fun _ -> assert false) fa fb fc fd) ()
  end
}

(****************************************************************************)
type 'a ref2 = 'a ref
type 'a ref = 'a ref2
class virtual ['ia,'a,'sa, 'inh, 'e, 'syn] ref_t =
object
  method virtual c_ref : 'inh -> 'a -> 'syn
end
let gcata_ref tr inh r = tr#c_ref inh !r 

class ['a, 'self] fmt_ref_t _ fa =
  object
    inherit [ 'inh, 'a, unit
            , 'inh, 'self, unit] ref_t
    constraint 'inh = Format.formatter
    method c_ref fmt a =
      Format.fprintf fmt "!(%a)" fa a
  end
class ['a, 'self] html_ref_t _ fa =
  object
    inherit [ 'inh, 'a, 'syn
            , 'inh, 'self, 'syn] ref_t
    constraint 'syn = HTML.viewer
    constraint 'inh = unit
    method c_ref () a = fa a
  end

let ref:
    ( ('ia, 'a, 'sa, 'inh, _, 'syn ) #ref_t ->
      'inh -> 'a ref -> 'syn
    , < fmt     : (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a ref -> unit;
        html    : ('a -> HTML.er) ->
                  'a ref -> HTML.er;
      >) t =
  {gcata   = gcata_ref;
   plugins = object
     method fmt     fa =
       gcata_ref (new @ref[fmt]     (fun _ -> assert false) fa)
     method html    fa =
       gcata_ref (new @ref[html]    (fun _ -> assert false) fa) ()
  end
}
(*** arrays *****************************************************************)
type 'a parray      = 'a array
type 'a array       = 'a parray

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] array_t = object
  method virtual do_array  : 'inh -> 'a array -> 'syn
end

let gcata_array tr inh subj = tr#do_array inh subj

class ['a, 'self] html_array_t fself fa =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @array
    method do_array () arr =
      HTML.ul @@ HTML.seq (
        [ HTML.string "array" ] @ List.map (fun x -> HTML.li @@ fa x) @@ Array.to_list arr
        )
  end

class ['a, 'self] show_array_t fself fa = object
  inherit [unit, 'a, string, unit, 'self, string] @array
  method do_array () arr =
    "[|" ^ (Array.fold_right (fun x s -> Printf.sprintf "%a; %s" fa x s) arr " |]")
end

class ['a, 'self] fmt_array_t fself fa = object
  inherit [Format.formatter, 'a, unit, Format.formatter, 'self, unit] @array

  method do_array fmt arr =
    Format.fprintf fmt "[| ";
    Array.iter (fun x -> Format.fprintf fmt "%a; " fa x) arr;
    Format.fprintf fmt " |]"
end

let array =
  { gcata = (fun _ _ -> failwith "arrays not implemented")
  ; plugins = object
      method fmt _fa fmt s = Format.fprintf fmt "<array>%!"
      method html _fa s = HTML.string "array HERE"
    end
  }

(****************************************************************************)
let show    t = t.plugins#show
let html    t = t.plugins#html
let gmap    t = t.plugins#gmap
let eval    t = t.plugins#eval
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare


