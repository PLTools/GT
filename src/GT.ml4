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

(** Implementation of transformation for standart types *)

open Printf

module Format = struct
  include Format
  let pp_print_unit  fmt () = pp_print_string fmt "()"
  let pp_print_int32 fmt n  = Format.pp_print_string fmt @@ Int32.format "%d" n
  let pp_print_int64 fmt n  = Format.pp_print_string fmt @@ Int64.format "%d" n
  let pp_print_nativeint fmt n = Format.pp_print_string fmt @@ Nativeint.format "%d" n
  let pp_print_string fmt s = fprintf fmt "\"%s\"" s
end

type ('a, 'b) t = {gcata : 'a; plugins : 'b}
let transform_gc gcata make_obj inh subj =
  let rec obj = lazy (make_obj fself)
  and fself inh x = gcata (Lazy.force obj) inh x in
  fself inh subj

let transform  bundle = transform_gc  bundle.gcata

let lift f _ = f
let id x  = x

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

(** {1 List } *)

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

class ['a, 'self] html_list_t fa fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @list
    method c_Nil  _      = View.empty
    method c_Cons _ x xs =

      HTML.ul @@ HTML.seq (
        [ HTML.string "list" ] @ List.map (fun x -> HTML.li @@ fa () x) (x::xs)
        )
(*      View.concat (fa x) (match xs with [] -> View.empty | xs -> HTML.li (fself () xs)) *)
  end

class ['a, 'self] show_list_t fa fself =
  object
    inherit [unit, 'a, string, unit, 'self, string] @list
    method c_Nil  _      = ""
    method c_Cons _ x xs = (fa () x) ^ (match xs with [] -> "" | _ -> "; " ^ (fself () xs))
  end

class ['a, 'self] fmt_list_t fa fself =
  object
    inherit ['inh, 'a, unit, 'inh, 'self, unit] @list
    constraint 'inh = Format.formatter
    method c_Nil  _      = ()
    method c_Cons fmt x xs =
      Format.fprintf fmt "%a;@,@ %a" fa x fself xs
  end

class ['a, 'sa, 'self, 'syn ] gmap_list_t fa fself =
  object
    constraint 'syn = 'sa list
    inherit [unit, 'a, 'sa, unit, 'self, 'syn] @list
    method c_Nil  _ = []
    method c_Cons _ x xs = (fa () x) :: (fself () xs)
  end
class ['a, 'sa, 'self, 'syn, 'env ] eval_list_t fa fself =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'sa list] @list
    method c_Nil  _ = []
    method c_Cons env x xs = (fa env x) :: (fself env xs)
  end
class ['a, 'sa, 'self, 'syn, 'env ] stateful_list_t fa fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa list] @list
    method c_Nil  env = (env, [])
    method c_Cons env0 x xs : 'env * 'sa list =
      let env1,h  = fa    env0 x  in
      let env2,tl = fself env1 xs in
      env2, (h::tl)
  end

class ['a, 'syn, 'self] foldl_list_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @list
    method c_Nil  s = s
    method c_Cons s x xs = fself  (fa s x) xs
  end

class ['a, 'syn, 'self] foldr_list_t fa fself =
  object
    inherit ['a, 'syn, 'self] @list[foldl] fa fself
    method c_Cons s x xs = fa (fself s xs) x
  end

class ['a, 'self] eq_list_t fa fself =
  object
    inherit ['a, 'a, bool, 'a list, 'self, bool] @list
    method c_Nil inh  = (inh = [])
    method c_Cons inh x xs =
      match inh with
      | y::ys -> fa y x && fself ys xs
      | _ -> false
  end

class ['a, 'self] compare_list_t fa fself =
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
              html    : ('a -> HTML.viewer) -> 'a list -> HTML.viewer;
              gmap    : ('a -> 'b)          -> 'a list -> 'b list;

              fmt     : (Format.formatter -> 'a -> unit) ->
                        Format.formatter -> 'a list -> unit;
              eval    : ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list;
              stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a list -> 'env * 'b list;
              foldl   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              foldr   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
              eq      : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool;
              compare : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison;
            >) t =
  {gcata   = gcata_list;
   plugins = object
               method show fa l =
                 sprintf "[%a]" (transform_gc gcata_list (new @list[show] (lift fa))) l
               method html    fa   = transform_gc gcata_list (new @list[html] (lift fa)) ()
               method gmap    fa   = transform_gc gcata_list (new @list[gmap] (lift fa)) ()

               method fmt fa inh l =
                 Format.fprintf inh "[@[%a@]]@,"
                   (transform_gc gcata_list (new @list[fmt] fa)) l

               method stateful fa  = transform_gc gcata_list (new @list[stateful] fa)
               method eval     fa  = transform_gc gcata_list (new @list[eval] fa)
               method eq       fa  = transform_gc gcata_list (new @list[eq] fa)
               method compare  fa  = transform_gc gcata_list (new @list[compare] fa)
               method foldl    fa  = transform_gc gcata_list (new @list[foldl] fa)
               method foldr    fa  = transform_gc gcata_list (new @list[foldr] fa)
             end
  }


(** {1 Lazy values } *)

module Lazy =
  struct

    type ('a, 'b) t' = ('a, 'b) t

    include Lazy

    class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] t_t = object
        method virtual t_t : 'inh -> 'a t -> 'syn
      end
    let gcata_t tr inh subj = tr#t_t inh subj
    let gcata_lazy = gcata_t

    class ['a, 'self ] show_t_t fa _fself =
      object
        inherit [unit, 'a, string, unit, 'self, string ] @t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

    class ['a, 'self ] html_t_t fa _fself =
      object
        inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer ] @t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

    class ['a, 'sa, 'self, 'syn ] gmap_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit [unit, 'a, 'sa, unit, 'self, 'syn ] @t
        method t_t inh subj = lazy (fa () @@ Lazy.force subj)
      end

    class ['a, 'sa, 'self, 'syn, 'env ] eval_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit ['env, 'a, 'sa, 'env, 'self, 'syn ] @t
        method t_t env subj = lazy (fa env @@ Lazy.force subj)
      end

    class ['a, 'sa, 'self, 'syn, 'env ] stateful_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'syn ] @t
        method t_t env subj =
          let (env1, r) = fa env @@ Lazy.force subj
          in env1, Lazy.from_fun (fun () -> r)
          (* THE SAME AS eval *)
      end

    class ['a, 'syn, 'self ] foldl_t_t fa _fself =
      object
        inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn ] @t
        method t_t inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'syn, 'self ] foldr_t_t fa fself =
      object
        inherit ['a, 'syn, 'self ] @t[foldl] fself fa
      end

    class ['a, 'self ] eq_t_t fa _fself =
      object
        inherit ['a, 'a, bool, 'a t, 'self, bool ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    class ['a, 'self ] compare_t_t fa _fself =
      object
        inherit ['a, 'a, comparison, 'a t, 'self, comparison ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    let t : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #t_t -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      -> 'a t -> string;
               html    : ('a -> HTML.viewer) -> 'a t -> HTML.viewer;
               gmap    : ('a -> 'b)          -> 'a t -> 'b t;

               eval    : ('env -> 'a -> 'b) -> 'env -> 'a t -> 'b t;
               stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a t -> 'env * 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >) t' =
      let fself _ _ = assert false in
      {gcata   = gcata_lazy;
       plugins = object
                   method show     fa  = gcata_lazy (new @t[show] fself (lift fa)) ()
                   method html     fa  = gcata_lazy (new @t[html] fself (lift fa)) ()
                   method gmap     fa  = gcata_lazy (new @t[gmap] fself (lift fa)) ()

                   method eval     fa  = gcata_lazy (new @t[eval] fself fa)
                   method stateful fa  = gcata_lazy (new @t[stateful] fself fa)
                   method eq      fa   = gcata_lazy (new @t[eq] fself fa)
                   method compare fa   = gcata_lazy (new @t[compare] fself fa)
                   method foldl   fa   = gcata_lazy (new @t[foldl] fself fa)
                   method foldr   fa   = gcata_lazy (new @t[foldr] fself fa)
                 end
      }
  end

(** {1 Option } *)
(* ************************************************************************* *)
type 'a poption = 'a option
type 'a option = 'a poption

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] option_t =
  object
    method virtual c_None :   'inh -> 'a option       -> 'syn
    method virtual c_Some :   'inh -> 'a option -> 'a -> 'syn
  end

let gcata_option tr inh subj =
  match subj with
  | None   -> tr#c_None inh subj
  | Some x -> tr#c_Some inh subj x

class ['a, 'self] show_option_t fa _fself =
  object
    inherit [ unit, 'a, string, unit, 'self, string] @option
    method c_None () _   = "None"
    method c_Some () _ x = Printf.sprintf "Some (%a)" fa x
  end
class ['a, 'self] html_option_t fa _fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @option
    method c_None () _   = HTML.string "None"
    method c_Some () _ x = View.concat (HTML.string "Some") (HTML.ul (fa () x))
  end

class ['a, 'self] fmt_option_t fa _fself =
  object
    inherit [ Format.formatter, 'a, unit, Format.formatter, 'self, unit] @option
    method c_None fmt _   = Format.fprintf fmt "None"
    method c_Some fmt _ x = Format.fprintf fmt "Some (%a)" fa x
  end

class ['a, 'sa, 'self, 'syn ] gmap_option_t fa _fself =
  object
    constraint 'syn = 'sa option
    inherit [unit, 'a, 'sa, unit, 'self, 'syn ] @option
    method c_None () _ = None
    method c_Some () _ x = Some (fa () x)
  end

class ['a, 'sa, 'self, 'syn, 'env ] eval_option_t fa _fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'sa option] @option
    method c_None _   _   = None
    method c_Some env _ x = Some (fa env x)
  end

class ['a, 'sa, 'self, 'syn, 'env ] stateful_option_t fa _fself =
  object
    constraint 'syn = 'sa option
    inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'syn ] @option
    method c_None env _   = (env,None)
    method c_Some env _ x =
      let env1,r = fa env x in
      (env1, Some r)
  end

class ['a, 'syn, 'self] foldl_option_t fa _fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @option
    method c_None s _   = s
    method c_Some s _ x = fa s x
  end

class ['a, 'syn, 'self] foldr_option_t fa _fself =
  object
    inherit ['a, 'syn, 'self] @option[foldl] fa _fself
  end

class ['a, 'self] eq_option_t fa _fself =
  object
    inherit ['a, 'a, bool, 'a option, 'self, bool] @option
    method c_None inh _   = (inh = None)
    method c_Some inh _ x =
      match inh with
      | Some y -> fa y x
      | _ -> false
  end

class ['a, 'self] compare_option_t fa _fself =
  object
    inherit ['a, 'a, comparison, 'a option, 'self, comparison] @option
    method c_None inh _ = match inh with
      | None -> EQ
      | _  -> GT
    method c_Some inh _ x =
      match inh with
      | None -> LT
      | Some y -> fa y x
  end

let option : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #option_t -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
                html    : ('a -> HTML.viewer) -> 'a option -> HTML.viewer;
                gmap    : ('a -> 'b)          -> 'a option -> 'b option;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a option -> unit;
                stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a option -> 'env * 'b option;
                eval    : ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool;
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison;
              >) t =
  {gcata   = gcata_option;
   plugins = object
               method show     fa = transform_gc gcata_option (new @option[show] (lift fa)) ()
               method html     fa = transform_gc gcata_option (new @option[html] (lift fa)) ()
               method gmap     fa = transform_gc gcata_option (new @option[gmap] (lift fa)) ()

               method fmt      fa = transform_gc gcata_option (new @option[fmt] fa)
               method stateful fa = transform_gc gcata_option (new @option[stateful] fa)
               method eval     fa = transform_gc gcata_option (new @option[eval] fa)
               method eq       fa = transform_gc gcata_option (new @option[eq] fa)
               method compare  fa = transform_gc gcata_option (new @option[compare] fa)
               method foldl    fa = transform_gc gcata_option (new @option[foldl] fa)
               method foldr    fa = transform_gc gcata_option (new @option[foldr] fa)
             end
  }

(** So called antiphantom type *)
(* ************************************************************************* *)
(* Antiphantom type *)
type 'a free = 'a
class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] free_t = object
  method virtual c_Free : 'inh -> 'a free -> 'a -> 'syn
end
let gcata_free tr inh subj = tr#c_Free inh subj subj

class ['a, 'self] show_free_t fa _ =
  object
    inherit [unit, 'a, string, unit, 'self, string] free_t
    method c_Free () _ x = Printf.sprintf "(%a)" fa x
  end
class ['a, 'self] html_free_t fa _ =
  object
    inherit [unit, 'a, 'syn, unit, 'self, 'syn] free_t
    constraint 'syn = HTML.viewer
    method c_Free () _ x = fa () x
  end

class ['a, 'self] fmt_free_t fa _ =
  object
    inherit ['inh, 'a, unit, 'inh, 'self, unit] free_t
    constraint 'inh = Format.formatter
    method c_Free fmt _ x = Format.fprintf fmt "(%a)" fa x
  end

class ['a, 'sa, 'self, 'syn] gmap_free_t fa _ =
  object
    constraint 'syn = 'sa free
    inherit [unit, 'a, 'sa, unit, 'self, 'syn] free_t
    method c_Free () _ x = fa () x
  end

class ['a, 'sa, 'self, 'syn, 'env] eval_free_t fa _ =
  object
    constraint 'syn = 'sa free
    inherit ['emv, 'a, 'sa, 'env, 'self, 'syn] free_t
    method c_Free env _ x = fa env x
  end

class ['a, 'sa, 'self, 'syn, 'env] stateful_free_t fa _ =
  object
    constraint 'syn = 'env * 'sa free
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'syn] free_t
    method c_Free env _ x = fa env x
  end

class ['a, 'syn, 'self] foldl_free_t fa _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] free_t
    method c_Free inh _ x = fa inh x
  end

class ['a, 'syn, 'self] foldr_free_t fa _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] free_t
    method c_Free inh _ x = fa inh x
  end

class ['a, 'self] eq_free_t fa _ =
  object
    inherit ['a, 'a, bool, 'a free, 'self, bool] free_t
    method c_Free inh _ x = fa inh x
  end

class ['a, 'self] compare_free_t fa _ =
  object
    inherit ['a, 'a, 'syn, 'a free, 'self, 'syn] free_t
    constraint 'syn = comparison
    method c_Free z _ x = fa z x
  end

let free : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #free_t -> 'inh -> 'a free -> 'syn,
              < show    : ('a -> string)      -> 'a free -> string;
                html    : ('a -> HTML.viewer) -> 'a free -> HTML.viewer;
                gmap    : ('a -> 'c)          -> 'a free -> 'c free;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a free -> unit;
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
       method show     fa = transform_gc gcata_free (new @free[show] (lift fa)) ()
       method gmap     fa = transform_gc gcata_free (new @free[gmap] (lift fa)) ()
       method html     fa = transform_gc gcata_free (new @free[html] (lift fa)) ()

       method fmt      fa = transform_gc gcata_free (new @free[fmt] fa)
       method eval     fa = transform_gc gcata_free (new @free[eval]  fa)
       method stateful fa = transform_gc gcata_free (new @free[stateful] fa)
       method eq       fa = transform_gc gcata_free (new @free[eq] fa)
       method compare  fa = transform_gc gcata_free (new @free[compare] fa)
       method foldl    fa = transform_gc gcata_free (new @free[foldl] fa)
       method foldr    fa = transform_gc gcata_free (new @free[foldr] fa)
  end
  }

(* Pairs and other stuff without explicit structure *)
(*******************************************************************************)
(** Arrow *)
type ('a, 'b) arrow = 'a -> 'b

let gcata_arrow tr inh arr = tr#c_Arrow inh arr

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] arrow_t =
  object
    method virtual c_Arrow : 'inh -> ('a,'b) arrow -> 'syn
  end

class ['a, 'b, 'self] show_arrow_t fa fb _ =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] arrow_t
    method c_Arrow () _ = Printf.sprintf "<function>"
  end

class ['a, 'b, 'self] fmt_arrow_t fa fb _ =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'self, unit] arrow_t
    constraint 'inh = Format.formatter
    method c_Arrow fmt _ = Format.fprintf fmt "<function>"
  end

class ['a, 'b, 'self] html_arrow_t fa fb _ =
  object
    inherit [unit, 'a, 'syn, unit, 'b, 'syn, unit, 'self, 'syn] arrow_t
    constraint 'syn = HTML.viewer
    method c_Arrow () _ = HTML.string "<arrow>"
  end

class ['a, 'sa, 'b, 'sb, 'self] gmap_arrow_t (fa: unit -> 'a -> 'sa) fb _ =
  object
    inherit [unit, 'a, 'sa, unit, 'b, 'sb, unit, 'self, ('sa, 'sb) arrow] arrow_t
    method c_Arrow () _ = failwith "gmap for arrows is not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'env, 'self] eval_arrow_t fa fb _ =
  object
    inherit ['env, 'a, 'sa, 'env, 'b, 'sb, 'env, 'self, ('sa, 'sb) arrow] arrow_t
    method c_Arrow _ _ = failwith "eval for arrows is not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'env, 'self] stateful_arrow_t fa fb _ =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'b, 'sb, 'env, 'self, 'env * ('sa, 'sb) arrow] arrow_t
    method c_Arrow _ _ = failwith "stateful for arrows is not implemented"
  end

class ['a, 'b, 'syn, 'self] foldl_arrow_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] arrow_t
    method c_Arrow _ _ = failwith "foldl for arrows is not implemented"
  end

class ['a, 'b, 'syn, 'self] foldr_arrow_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] arrow_t
    method c_Arrow _ _ = failwith "foldr for arrows is not implemented"
  end

class ['a, 'b, 'self] eq_arrow_t fa fb _ =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) arrow, 'self, bool] arrow_t
    method c_Arrow _ _ = failwith "eq for arrows is not implemented"
  end

class ['a, 'b, 'self] compare_arrow_t fa fb _ =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) arrow, 'self, 'syn] arrow_t
    constraint 'syn = comparison
    method c_Arrow _ _ = failwith "compare for arrows is not implemented"
  end

let arrow:
  ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, _, 'syn) #arrow_t -> 'inh -> ('a, 'b) arrow -> 'syn,
              < show    : ('a -> string) -> ('b -> string) ->
                          ('a, 'b) arrow -> string;
                html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) ->
                          ('a, 'b) arrow -> HTML.viewer;
                gmap    : ('a -> 'c) -> ('b -> 'd) ->
                          ('a, 'b) arrow -> ('c, 'd) arrow;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          (Format.formatter -> 'b -> unit) ->
                          Format.formatter -> ('a,'b) arrow -> unit;
                stateful: ('env -> 'a -> 'env * 'c) ->
                          ('env -> 'b -> 'env * 'd) ->
                          'env -> ('a, 'b) arrow -> 'env * ('c, 'd) arrow;
                eval    : ('env -> 'a -> 'c) -> ('env -> 'b -> 'd) ->
                          'env -> ('a, 'b) arrow -> ('c, 'd) arrow;
                foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) arrow -> 'c;
                foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) arrow -> 'c;
                eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) ->
                          ('a, 'b) arrow -> ('a, 'b) arrow -> bool;
                compare : ('a -> 'a -> comparison) -> ('b -> 'b -> comparison) ->
                          ('a, 'b) arrow -> ('a, 'b) arrow -> comparison;
              >) t =
  {gcata   = gcata_arrow;
   plugins =
     let tr  obj subj     = transform_gc gcata_arrow obj ()  subj in
     let tr1 obj inh subj = transform_gc gcata_arrow obj inh subj in
     object
       method show     fa fb = tr  (new @arrow[show] (lift fa) (lift fb))
       method html     fa fb = tr  (new @arrow[html] (lift fa) (lift fb))
       method gmap     fa fb = tr  (new @arrow[gmap] (lift fa) (lift fb))

       method fmt      fa fb = tr1 (new @arrow[fmt]  fa fb)
       method eval     fa fb = tr1 (new @arrow[eval] fa fb)
       method stateful fa fb = tr1 (new @arrow[stateful] fa fb)
       method eq       fa fb = tr1 (new @arrow[eq]   fa fb)
       method compare  fa fb = tr1 (new @arrow[compare] fa fb)
       method foldl    fa fb = tr1 (new @arrow[foldl] fa fb)
       method foldr    fa fb = tr1 (new @arrow[foldr] fa fb)
  end
 }

(*******************************************************************************)
(** Pair *)

type ('a, 'b) pair = 'a * 'b

let gcata_pair tr inh p =
  match p with
  | (a, b) -> tr#c_Pair inh p a b

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] pair_t =
  object
    method virtual c_Pair : 'inh -> 'a*'b -> 'a -> 'b -> 'syn
  end

class ['a, 'b, 'self] show_pair_t fa fb _ =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] pair_t
    method c_Pair () _ x y = Printf.sprintf "(%a, %a)" fa x fb y
  end
class ['a, 'b, 'self] fmt_pair_t fa fb _ =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'self, unit] pair_t
    constraint 'inh = Format.formatter
    method c_Pair fmt _ x y = Format.fprintf fmt "(%a,%a)" fa x fb y
  end

class ['a, 'b, 'self] html_pair_t fa fb _ =
  object
    inherit [unit, 'a, 'syn, unit, 'b, 'syn, unit, 'self, 'syn] pair_t
    constraint 'syn = HTML.viewer
    method c_Pair () _ x y =
      List.fold_left View.concat View.empty
         [ HTML.ul (fa () x)
         ; HTML.ul (fb () y)
         ]
  end

class ['a, 'sa, 'b, 'sb, 'self, 'syn] gmap_pair_t (fa: unit -> 'a -> 'sa) fb _ =
  object
    constraint 'syn = ('sa, 'sb) pair
    inherit [unit, 'a, 'sa, unit, 'b, 'sb, unit, 'self, 'syn] pair_t
    method c_Pair () _ x y = (fa () x, fb () y)
  end

class ['a, 'sa, 'b, 'sb, 'self, 'syn, 'env] eval_pair_t fa fb _ =
  object
    constraint 'syn = ('sa, 'sb) pair
    inherit ['env, 'a, 'sa, 'env, 'b, 'sb, 'env, 'self, 'syn] pair_t
    method c_Pair env _ x y = (fa env x, fb env y)
  end
class ['a, 'sa, 'b, 'sb, 'self, 'syn, 'env] stateful_pair_t fa fb _ =
  object
    constraint 'syn = ('sa, 'sb) pair
    inherit ['env, 'a, 'env * 'sa, 'env, 'b, 'sb, 'env, 'self, 'env * 'syn ] pair_t
    method c_Pair env _ x y =
      let env1,l = fa env x in
      let env2,r = fb env y in
      env, (l,r)
  end

class ['a, 'b, 'syn, 'self] foldl_pair_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] pair_t
    method c_Pair s _ x y = fb (fa s x) y
  end

class ['a, 'b, 'syn, 'self] foldr_pair_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] pair_t
    method c_Pair s _ x y = fa (fb s y) x
  end

class ['a, 'b, 'self] eq_pair_t fa fb _ =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) pair, 'self, bool] pair_t
    method c_Pair inh _ x y =
      match inh with
      (z, t) -> fa z x && fb t y
  end

class ['a, 'b, 'self] compare_pair_t fa fb _ =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) pair, 'self, 'syn] pair_t
    constraint 'syn = comparison
    method c_Pair (z,t) _ x y = (match fa z x with EQ -> fb t y | c -> c)
  end

let pair:
  ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, _, 'syn) #pair_t -> 'inh -> ('a, 'b) pair -> 'syn,
              < show    : ('a -> string) -> ('b -> string) ->
                          ('a, 'b) pair -> string;
                html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) ->
                          ('a, 'b) pair -> HTML.viewer;
                gmap    : ('a -> 'c) -> ('b -> 'd) ->
                          ('a, 'b) pair -> ('c, 'd) pair;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          (Format.formatter -> 'b -> unit) ->
                          Format.formatter -> ('a,'b) pair -> unit;
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
   plugins =
     let tr  obj subj     = transform_gc gcata_pair obj ()  subj in
     let tr1 obj inh subj = transform_gc gcata_pair obj inh subj in
     object
       method show     fa fb = tr  (new @pair[show] (lift fa) (lift fb))
       method html     fa fb = tr  (new @pair[html] (lift fa) (lift fb))
       method gmap     fa fb = tr  (new @pair[gmap] (lift fa) (lift fb))

       method fmt      fa fb = tr1 (new @pair[fmt]  fa fb)
       method eval     fa fb = tr1 (new @pair[eval] fa fb)
       method stateful fa fb = tr1 (new @pair[stateful] fa fb)
       method eq       fa fb = tr1 (new @pair[eq]   fa fb)
       method compare  fa fb = tr1 (new @pair[compare] fa fb)
       method foldl    fa fb = tr1 (new @pair[foldl] fa fb)
       method foldr    fa fb = tr1 (new @pair[foldr] fa fb)
  end
 }

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] tuple2_t = object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] pair_t
end
let gcata_tuple2 = gcata_pair
let tuple2 = pair

(* Just aliases *)
class ['a, 'b, 'self] show_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'self] show_pair_t fa fb fself
end
class ['a, 'b, 'self] fmt_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'self] fmt_pair_t fa fb fself
end
class ['a, 'b, 'self] html_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'self] html_pair_t fa fb fself
end
class ['a, 'b, 'self] compare_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'self] compare_pair_t fa fb fself
end
class ['a, 'b, 'self] eq_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'self] eq_pair_t fa fb fself
end
class ['a, 'b, 'syn, 'self] foldl_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'syn, 'self] foldl_pair_t fa fb fself
end
class ['a, 'b, 'syn, 'self] foldr_tuple2_t fa fb fself = object
  inherit [ 'a, 'b, 'syn, 'self] foldr_pair_t fa fb fself
end

class ['a, 'a2, 'b, 'b2, 'self, 'syn] gmap_tuple2_t fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'self, 'syn] gmap_pair_t fa fb fself
end
class ['a, 'a2, 'b, 'b2, 'self, 'syn, 'env] eval_tuple2_t fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'self, 'syn, 'env] eval_pair_t fa fb fself
end
class ['a, 'a2, 'b, 'b2, 'self, 'syn, 'env] stateful_tuple2_t fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'self, 'syn, 'env] stateful_pair_t fa fb fself
end

(*******************************************************************************)
(** {1 Tuples of size 3} *)

type ('a,'b,'c) triple = 'a * 'b * 'c
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] triple_t = object
  method virtual c_Triple : 'inh -> 'a -> 'b -> 'c -> 'syn
end
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] tuple3_t = object
  inherit     ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] triple_t
end
let gcata_triple tr inh (a,b,c) = tr#c_Triple inh a b c
let gcata_tuple3 = gcata_triple

class ['a, 'b, 'c, 'self] show_triple_t fa fb fc _ =
  object
    inherit [ unit, 'a, string
            , unit, 'b, string
            , unit, 'c, string
            , unit, 'self, string] @triple
    method c_Triple () x y z = Printf.sprintf "(%a, %a, %a)"
      fa x fb y fc z
end

class ['a, 'b, 'c, 'self] fmt_triple_t fa fb fc _ =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'c, unit, 'inh, 'self, unit] triple_t
    constraint 'inh = Format.formatter
    method c_Triple fmt x y z = Format.fprintf fmt "(%a,%a,%a)" fa x fb y fc z
  end

class ['a, 'b, 'c, 'self] html_triple_t fa fb fc _ =
  object
    inherit [ unit, 'a, 'syn, unit, 'b, 'syn, unit, 'c, 'syn
            , unit, 'self, 'syn] triple_t
    constraint 'syn = HTML.viewer
    method c_Triple () x y z =
      List.fold_left View.concat View.empty
         [ HTML.string "("
         ; HTML.ul (fa () x)
         ; HTML.string ", "
         ; HTML.ul (fb () y)
         ; HTML.string ", "
         ; HTML.ul (fc () z)
         ; HTML.string ")"]
  end

class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn] gmap_triple_t fa fb fc _ =
  object
    constraint 'syn = ('a2,'b2,'c2) triple
    inherit [ unit, 'a, 'a2
            , unit, 'b, 'b2
            , unit, 'c, 'c2
            , unit, 'self, 'syn ] @triple
    method c_Triple () x y z = ( fa () x, fb () y, fc () z)
end

class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env] eval_triple_t fa fb fc _ =
  object
    constraint 'syn = ('a2,'b2,'c2) triple
    inherit [ 'env, 'a, 'a2
            , 'env, 'b, 'b2
            , 'env, 'c, 'c2
            , 'env, 'self, 'syn ] @triple
    method c_Triple e x y z = ( (fa e x), (fb e y), (fc e z) )
end

class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env ] stateful_triple_t fa fb fc _ =
  object
    constraint 'syn = ('a2,'b2,'c2) triple
    inherit [ 'env, 'a, 'env * 'a2
            , 'env, 'b, 'env * 'b2
            , 'env, 'c, 'env * 'c2
            , 'env, 'self, 'env * 'syn ] @triple
    method c_Triple env0 x y z =
      let env1,a = fa env0 x in
      let env2,b = fb env1 y in
      let env3,c = fc env2 z in
      env3, (a,b,c)
end
class ['a, 'b, 'c, 'self] compare_triple_t fa fb fc _ =
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

class ['a, 'b, 'c, 'self] eq_triple_t fa fb fc _ =
  object
    inherit [ 'a, 'a, bool
            , 'b, 'b, bool
            , 'c, 'c, bool
            , ('a, 'b, 'c) triple, 'self, bool] @triple
    method c_Triple inh x y z =
      match inh with
      (a, b, c) -> fa a x && fb b y && fc c z
  end

class ['a, 'b, 'c, 'syn, 'self] foldl_triple_t fa fb fc _ =
  object
    inherit [ 'syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'c, 'syn
            , 'syn, 'self, 'syn] @triple
    method c_Triple s x y z = fc (fb (fa s x) y) z
  end

class ['a, 'b, 'c, 'syn, 'self] foldr_triple_t fa fb fc _ =
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
    , < show    : ('a -> string) ->
                  ('b -> string) ->
                  ('c -> string) ->
                  ('a, 'b, 'c) triple  -> string;
        gmap    : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) ->
                  ('a, 'b, 'c) triple -> ('d, 'e, 'f) triple;
        html    : ('a -> HTML.er) ->
                  ('b -> HTML.er) ->
                  ('c -> HTML.er) ->
                  ('a, 'b, 'c) triple -> HTML.er;

        fmt     : (Format.formatter -> 'a -> unit) ->
                  (Format.formatter -> 'b -> unit) ->
                  (Format.formatter -> 'c -> unit) ->
                  Format.formatter -> ('a, 'b, 'c) triple -> unit;
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
        foldr   : ('syn -> 'a -> 'syn) ->
                  ('syn -> 'b -> 'syn) ->
                  ('syn -> 'c -> 'syn) ->
                  'syn ->
                  ('a, 'b, 'c) triple ->
                  'syn;
      >) t =
  {gcata   = gcata_triple;
   plugins =
     let tr  obj subj     = transform_gc gcata_triple obj  () subj in
     let tr1 obj inh subj = transform_gc gcata_triple obj inh subj in
     object
       method show     fa fb fc = tr  (new @triple[show] (lift fa) (lift fb) (lift fc))
       method html     fa fb fc = tr  (new @triple[html] (lift fa) (lift fb) (lift fc))
       method gmap     fa fb fc = tr  (new @triple[gmap] (lift fa) (lift fb) (lift fc))

       method fmt      fa fb fc = tr1 (new @triple[fmt]  fa fb fc)
       method eval     fa fb fc = tr1 (new @triple[eval] fa fb fc)
       method stateful fa fb fc = tr1 (new @triple[stateful] fa fb fc)
       method eq       fa fb fc = tr1 (new @triple[eq]   fa fb fc)
       method compare  fa fb fc = tr1 (new @triple[compare] fa fb fc)
       method foldl    fa fb fc = tr1 (new @triple[foldl] fa fb fc)
       method foldr    fa fb fc = tr1 (new @triple[foldr] fa fb fc)
  end
 }

let tuple3 = triple

class ['a, 'b, 'c, 'self] show_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] show_triple_t fa fb fc fself
end
class ['a, 'b, 'c, 'self] fmt_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[fmt] fa fb fc fself
end
class ['a, 'b, 'c, 'self] html_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[html] fa fb fc fself
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn] gmap_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn] @triple[gmap] fa fb fc fself
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env] eval_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env] @triple[eval] fa fb fc fself
end
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env] stateful_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'self, 'syn, 'env] @triple[stateful] fa fb fc fself
end
class ['a, 'b, 'c, 'self] compare_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] compare_triple_t fa fb fc fself
end
class ['a, 'b, 'c, 'self] eq_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[eq] fa fb fc fself
end
class ['a, 'b, 'c, 'syn, 'self] foldl_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] @triple[foldl] fa fb fc fself
end
class ['a, 'b, 'c, 'syn, 'self] foldr_tuple3_t fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] @triple[foldr] fa fb fc fself
end

(*******************************************************************************)
(** {1 Tuples of size 4} *)

type ('a, 'b, 'c, 'd) tuple4 = 'a * 'b * 'c * 'd
class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'id,'d,'sd, 'inh, 'e, 'syn] tuple4_t =
object
  method virtual c_tuple4 : 'inh -> 'a -> 'b -> 'c -> 'd -> 'syn
end
let gcata_tuple4 tr inh (a,b,c,d) = tr#c_tuple4 inh a b c d

class ['a, 'b, 'c, 'd, 'self] fmt_tuple4_t fa fb fc fd _ =
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
class ['a, 'b, 'c, 'd, 'self] html_tuple4_t fa fb fc fd _ =
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
         ; HTML.ul (fa () x)
         ; HTML.string ", "
         ; HTML.ul (fb () y)
         ; HTML.string ", "
         ; HTML.ul (fc () z)
         ; HTML.string ")"
         ; HTML.ul (fd () d)
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
        html    : ('a -> HTML.er) ->
                  ('b -> HTML.er) ->
                  ('c -> HTML.er) ->
                  ('d -> HTML.er) ->
                  ('a, 'b, 'c, 'd) tuple4 -> HTML.er;
      >) t =
  {gcata   = gcata_tuple4;
   plugins =
     let tr  obj subj     = transform_gc gcata_tuple4 obj  () subj in
     let tr1 obj inh subj = transform_gc gcata_tuple4 obj inh subj in
     object
       method html     fa fb fc fd = tr  (new @tuple4[html] (lift fa) (lift fb) (lift fc) (lift fd))
       method fmt      fa fb fc fd = tr1 (new @tuple4[fmt]  fa fb fc fd)
     end
}

(****************************************************************************)
(* {1 Mutable references} *)
type 'a ref2 = 'a ref
type 'a ref = 'a ref2
class virtual ['ia,'a,'sa, 'inh, 'e, 'syn] ref_t =
object
  method virtual c_ref : 'inh -> 'a -> 'syn
end
let gcata_ref tr inh r = tr#c_ref inh !r

class ['a, 'self] fmt_ref_t fa _ =
  object
    inherit [ 'inh, 'a, unit
            , 'inh, 'self, unit] ref_t
    constraint 'inh = Format.formatter
    method c_ref fmt a =
      Format.fprintf fmt "!(%a)" fa a
  end
class ['a, 'self] html_ref_t fa _ =
  object
    inherit [ 'inh, 'a, 'syn
            , 'inh, 'self, 'syn] ref_t
    constraint 'syn = HTML.viewer
    constraint 'inh = unit
    method c_ref () a = fa () a
  end
class ['a, 'self] show_ref_t fa _ =
  object
    inherit [ 'inh, 'a, 'syn
            , 'inh, 'self, 'syn] ref_t
    constraint 'syn = string
    constraint 'inh = unit
    method c_ref () a = fa () a
  end

let ref:
    ( ('ia, 'a, 'sa, 'inh, _, 'syn ) #ref_t ->
      'inh -> 'a ref -> 'syn
    , < html    : ('a -> HTML.er) ->  'a ref -> HTML.er;
        show    : ('a -> string)  ->  'a ref -> string;

        fmt     : (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a ref -> unit;
      >) t =
  {gcata   = gcata_ref;
   plugins = object
     method show    fa = transform_gc gcata_ref (new @ref[show] (lift fa)) ()
     method html    fa = transform_gc gcata_ref (new @ref[html] (lift fa)) ()

     method fmt     fa = transform_gc gcata_ref (new @ref[fmt]  fa)
  end
}
(*** arrays *****************************************************************)
(* TODO: array are not really implemented *)
(* {1 Arrays (N.B. WIP) } *)
type 'a parray      = 'a array
type 'a array       = 'a parray

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] array_t = object
  method virtual do_array  : 'inh -> 'a array -> 'syn
end

let gcata_array tr inh subj = tr#do_array inh subj

class ['a, 'self] show_array_t fa fself = object
  inherit [unit, 'a, string, unit, 'self, string] @array
  method do_array () arr =
    "[|" ^ (Array.fold_right
              (fun x s -> Printf.sprintf "%a; %s" fa x s) arr " |]")
end

class ['a, 'sa, 'self] gmap_array_t fa fself =
  object
    inherit [unit, 'a, 'sa, unit, 'self, 'sa array] @array
    method do_array () arr = Array.map (fa ()) arr
  end
class ['a, 'self] html_array_t fa fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @array
    method do_array () arr =
      HTML.ul @@ HTML.seq (
        [ HTML.string "array" ] @ List.map (fun x -> HTML.li @@ fa () x) @@ Array.to_list arr
        )
  end

class ['a, 'self] fmt_array_t fa fself = object
  inherit [Format.formatter, 'a, unit, Format.formatter, 'self, unit] @array

  method do_array fmt arr =
    Format.fprintf fmt "[| ";
    Array.iter (fun x -> Format.fprintf fmt "%a; " fa x) arr;
    Format.fprintf fmt " |]"
end

class ['a, 'sa, 'env, 'self] eval_array_t fa fself =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'sa array] @array
    method do_array env arr = Array.map (fa env) arr
  end
class ['a, 'sa, 'env, 'self] stateful_array_t fa fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa array] @array
    method do_array env0 arr =
      let n = Array.length arr in
      if n = 0 then ([||], env0)
      else
        let (x1,env1) = fa env0 (Array.get arr 0) in
        let env = Pervasives.ref env1 in
        let ans = Array.make n x1 in
        for i=1 to n - 1 do
          let (x,env2) = fa !env (Array.get arr i) in
          env := env2;
          Array.set ans i x
        done;
        (!env, ans)
  end

class ['a, 'syn, 'self] foldl_array_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @array
    method do_array env arr = Array.fold_left fa env arr
  end

class ['a, 'syn, 'self] foldr_array_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] @array
    method do_array env arr = Array.fold_right (fun x acc -> fa acc x) arr env
  end

class ['a, 'self] eq_array_t fa fself =
  object
    inherit ['a, 'a, bool, 'a array, 'self, bool] @array
    method do_array env arr =
      let n = Array.length arr in
      (Array.length env = n) &&
      (let ans = Pervasives.ref true in
       for i=0 to n do
         ans := !ans && (fa (Array.get env i) (Array.get arr i) )
       done;
       !ans
      )
  end

class ['a, 'self] compare_array_t fa fself =
  object
    inherit ['a, 'a, comparison, 'a array, 'self, comparison] @array
    method do_array env arr =
      let n = Array.length arr in
      if Array.length env < n then LT else
      (let ans = Pervasives.ref EQ in
       for i=0 to n do
         ans := chain_compare !ans (fun () -> fa (Array.get env i) (Array.get arr i))
       done;
       !ans
      )
  end

let array =
  { gcata = gcata_array
  ; plugins =
      let tr  obj fa   s = transform_gc gcata_array (obj fa) () s in
      let tr1 obj fa i s = transform_gc gcata_array (obj fa)  i s in
      object
        method show fa  = tr (new @array[show]) (lift fa)
        method gmap fa  = tr (new @array[gmap]) (lift fa)
        method html fa  = tr (new @array[html]) (lift fa)

        method fmt      fa = tr1 (new @array[fmt]) fa
        method eval     fa = tr1 (new @array[eval]) fa
        method stateful fa = tr1 (new @array[stateful]) fa
        method compare  fa = tr1 (new @array[compare]) fa
        method eq       fa = tr1 (new @array[eq]) fa
        method foldl    fa = tr1 (new @array[foldl]) fa
        method foldr    fa = tr1 (new @array[foldr]) fa
    end
  }

(*** bytes *****************************************************************)
(* {1 Bytes (mutable string) } *)
type bytes = Bytes.t

class virtual ['inh, 'self, 'syn] bytes_t = object
  method virtual do_bytes  : 'inh -> bytes -> 'syn
end

let gcata_bytes tr inh subj = tr#do_bytes inh subj

class ['self] html_bytes_t fself =
  object
    inherit [unit, 'self, HTML.viewer] @bytes
    method do_bytes () arr =
      HTML.string @@ Bytes.to_string arr
  end

class ['self] show_bytes_t fself = object
  inherit [ unit, 'self, string] @bytes
  method do_bytes () = Bytes.to_string
end
class ['self] gmap_bytes_t fself = object
  inherit [unit, 'self, bytes] @bytes
  method do_bytes () arr = arr
end

class ['self] fmt_bytes_t fself = object
  inherit [Format.formatter, 'self, unit] @bytes

  method do_bytes fmt arr =
    Format.fprintf fmt "%S" (Bytes.to_string arr)
end
class ['env, 'self] eval_bytes_t fself =
  object
    inherit [ 'env, 'self, bytes] @bytes
    method do_bytes env arr = arr
  end
class ['env, 'self] stateful_bytes_t fself =
  object
    inherit ['env, 'self, 'env * bytes] @bytes
    method do_bytes env0 arr = (env0,arr)
  end

class ['syn, 'self] foldl_bytes_t fself =
  object
    inherit ['syn, 'self, 'syn] @bytes
    method do_bytes env _ = env
  end

class ['syn, 'self] foldr_bytes_t fself =
  object
    inherit ['syn, 'self, 'syn] @bytes
    method do_bytes env _ = env
  end

class ['self] eq_bytes_t fself =
  object
    inherit [bytes, 'self, bool] @bytes
    method do_bytes env arr = (Bytes.compare env arr = 0)
  end

class ['self] compare_bytes_t fself =
  object
    inherit [bytes, 'self, comparison] @bytes
    method do_bytes env arr =
      let c = Bytes.compare env arr in
      if c < 0 then LT
      else if c = 0 then EQ
      else GT
  end

let bytes =
  { gcata = gcata_bytes
  ; plugins =
      let tr  obj    s = gcata_bytes (obj (fun _ _ -> assert false) ) () s in
      let tr1 obj i  s = gcata_bytes (obj (fun _ _ -> assert false) ) i  s in
      object
        method show   = tr (new @bytes[show])
        method gmap   = tr (new @bytes[gmap])
        method html   = tr (new @bytes[html])

        method fmt    = tr1 (new @bytes[fmt])

        method eval   = tr1 (new @bytes[eval])
        method stateful = tr1 (new @bytes[stateful])
        method compare  = tr1 (new @bytes[compare])
        method eq       = tr1 (new @bytes[eq])
        method foldl    = tr1 (new @bytes[foldl])
        method foldr    = tr1 (new @bytes[foldr])
    end
  }

(****************************************************************************)
let show    t = t.plugins#show
let html    t = t.plugins#html
let gmap    t = t.plugins#gmap

let fmt     t = t.plugins#fmt
let eval    t = t.plugins#eval
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare
let stateful t = t.plugins#stateful
let eval     t = t.plugins#eval
