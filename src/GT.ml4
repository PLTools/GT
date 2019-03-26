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

open Printf

module Format = struct
  include Format
  let pp_print_unit  fmt () = pp_print_string fmt "()"
  let pp_print_int32 fmt n  = Format.pp_print_string fmt @@ Int32.format "%d" n
  let pp_print_int64 fmt n  = Format.pp_print_string fmt @@ Int64.format "%d" n
  let pp_print_nativeint fmt n = Format.pp_print_string fmt @@ Nativeint.format "%d" n
  let pp_print_string fmt s = fprintf fmt "\"%s\"" s
end

type ('a, 'b) t = { gcata : 'a; plugins : 'b }
let transform_gc gcata make_obj inh subj =
  let rec obj = lazy (make_obj fself)
  and fself inh x = gcata (Lazy.force obj) inh x in
  fself inh subj

let transform  bundle = transform_gc  bundle.gcata

module type FixVR = sig
  type 'a s
  type fn = { call : 'a. 'a s -> 'a }
  val fixv : (fn -> fn) -> fn
end
module FixV(Sym: sig type 'a i end) =
(struct
  type 'a s = 'a Sym.i
  type fn = { call: 'a. 'a Sym.i -> 'a }
  (* ∀t.((∀α.α t → α) → (∀α.α t → α)) → (∀α.α t → α) *)
  let fixv f =
    let rec g = { call = fun x -> (f g).call x } in
    g
end: FixVR with type 'a s = 'a Sym.i)


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

(* ************************************************************************* *)
(** Standart types go there *)
type 'a plist      = 'a list
type 'a list       = 'a plist

(* TODO: add switch to customize methods' names generated from constructors *)
class virtual ['ia,'a,'sa,'inh,'extra,'syn] list_t =
  object
    method virtual  c_Nil : 'inh -> 'a list -> 'syn
    method virtual  c_Cons : 'inh -> 'a list -> 'a -> 'a list -> 'syn
  end
let gcata_list tr inh subj =
  match subj with
  | [] -> tr#c_Nil inh subj
  | _x__001_::_x__002_ -> tr#c_Cons inh subj _x__001_ _x__002_

module type IndexResult_list  = sig
  type 'a result
  type 'd i = List: ('a result -> 'a list result) i
end
module Index_list(S:sig type 'a result end) =
  struct
    type 'a result = 'a S.result
    type 'xxx i =
      | List: ('a result -> 'a list result) i
  end
module type IndexResult2_list  =
  sig
    type ('a, 'b) result
    type 'xxx i =
      | List: (('a, 'a2) result -> ('a list, 'a2 list) result) i
  end
module Index2_list(S:sig type ('a, 'b) result end) =
  struct
    type ('a, 'b) result = ('a, 'b) S.result
    type 'd i =
      | List: (('a, 'a2) result -> ('a list, 'a2 list) result) i
  end
module type IndexResult_fold_list  =
  sig
    type ('a, 'b) result
    type 'xxx i =
      | List: (('a, 'a2) result -> ('a list, 'a2) result) i
  end
module Index_fold_list(S:sig type ('a, 'b) result end) =
  struct
    type ('a, 'b) result = ('a, 'b) S.result
    type 'xxx i =
      | List: (('a, 'a2) result -> ('a list, 'a2) result) i
  end
module type IndexResult_stateful_list  =
  sig
    type ('env, 'a, 'b) result
    type 'xxx i =
      | List: (('env, 'a, 'a2) result -> ('env, 'a list, 'a2 list) result) i
  end
module Index_stateful_list(S:sig type ('env, 'a, 'b) result end) =
  struct
    type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
    type 'xxx i =
      | List: (('env, 'a, 'a2) result -> ('env, 'a list, 'a2 list) result) i
  end

module Ieq_list = (Index_list)(struct type 'a result = 'a -> 'a -> bool end)
module Fix_eq_list = (FixV)(Ieq_list)
class ['a,'extra_list] eq_list_t _ fa  fself_list =
  object
    inherit  ['a,'a,bool,'a list,'extra_list,bool] list_t
    method c_Nil inh___003_ _ =
      match inh___003_ with | [] -> true | other -> false
    method c_Cons inh___004_ _ _x__005_ _x__006_ =
      match inh___004_ with
      | _x__007_::_x__008_ ->
          (true && (fa _x__007_ _x__005_)) && (fself_list _x__008_ _x__006_)
      | other -> false
  end
let eq_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new eq_list_t) call fa) inh0 subj
let eq_list_fix =
  Fix_eq_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ieq_list.i) ->
             (match sym with | Ieq_list.List -> eq_list_0 f : a)
       })

module Icompare_list =
  (Index_list)(struct type 'a result = 'a -> 'a -> comparison end)
module Fix_compare_list = (FixV)(Icompare_list)
class ['a,'extra_list] compare_list_t _ fa  fself_list =
  object
    inherit  ['a,'a,comparison,'a list,'extra_list,comparison] list_t
    method c_Nil inh___009_ _ =
      match inh___009_ with | [] -> EQ | other -> compare_vari other []
    method c_Cons inh___010_ _ _x__011_ _x__012_ =
      match inh___010_ with
      | _x__013_::_x__014_ ->
          chain_compare
            (chain_compare EQ (fun () -> fa _x__013_ _x__011_))
            (fun () -> fself_list _x__014_ _x__012_)
      | other -> compare_vari other ((Obj.magic ()) :: (Obj.magic ()))
  end
let compare_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new compare_list_t) call fa) inh0 subj
let compare_list_fix =
  Fix_compare_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Icompare_list.i) ->
             (match sym with | Icompare_list.List -> compare_list_0 f :
             a)
       })

module Ifoldr_list =
  (Index_fold_list)(struct type ('a, 'b) result = 'b -> 'a -> 'b end)
module Fix_foldr_list = (FixV)(Ifoldr_list)
class ['a,'syn,'extra_list] foldr_list_t _ fa  fself_list =
  object
    inherit  ['syn,'a,'syn,'syn,'extra_list,'syn] list_t
    method c_Nil inh___015_ _ = inh___015_
    method c_Cons inh___016_ _ _x__017_ _x__018_ =
      fa (fself_list inh___016_ _x__018_) _x__017_
  end
let foldr_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new foldr_list_t) call fa) inh0 subj
let foldr_list_fix =
  Fix_foldr_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ifoldr_list.i) ->
             (match sym with | Ifoldr_list.List -> foldr_list_0 f : a)
       })

module Ifoldl_list =
  (Index_fold_list)(struct type ('a, 'b) result = 'b -> 'a -> 'b end)
module Fix_foldl_list = (FixV)(Ifoldl_list)
class ['a,'syn,'extra_list] foldl_list_t _ fa  fself_list =
  object
    inherit  ['syn,'a,'syn,'syn,'extra_list,'syn] list_t
    method c_Nil inh___019_ _ = inh___019_
    method c_Cons inh___020_ _ _x__021_ _x__022_ =
      fself_list (fa inh___020_ _x__021_) _x__022_
  end
let foldl_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new foldl_list_t) call fa) inh0 subj
let foldl_list_fix =
  Fix_foldl_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ifoldl_list.i) ->
             (match sym with | Ifoldl_list.List -> foldl_list_0 f : a)
       })
module Istateful_list =
  (Index_stateful_list)(struct
                          type ('env, 'a, 'b) result =
                            'env -> 'a -> ('env * 'b)
                        end)
module Fix_stateful_list = FixV(Istateful_list)
class ['a,'a_2,'env,'extra_list] stateful_list_t _ fa  fself_list =
  object
    inherit  ['env,'a,('env * 'a_2),'env,'extra_list,('env * 'a_2 list)]
      list_t
    method c_Nil inh___023_ _ = (inh___023_, [])
    method c_Cons inh___024_ _ _x__025_ _x__026_ =
      let (env1, _x__025__rez) = fa inh___024_ _x__025_ in
      let (env2, _x__026__rez) = fself_list env1 _x__026_ in
      (env2, (_x__025__rez :: _x__026__rez))
  end
let stateful_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new stateful_list_t) call fa) inh0 subj
let stateful_list_fix =
  Fix_stateful_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Istateful_list.i) ->
             (match sym with | Istateful_list.List -> stateful_list_0 f :
             a)
       })

module Ieval_list =
  (Index2_list)(struct type ('a, 'b) result = unit -> 'a -> 'b end)
module Fix_eval_list = (FixV)(Ieval_list)
class ['a,'a_2,'env,'extra_list] eval_list_t _ fa  fself_list =
  object
    inherit  ['env,'a,'a_2,'env,'extra_list,'a_2 list] list_t
    method c_Nil inh___027_ _ = []
    method c_Cons inh___028_ _ _x__029_ _x__030_ = (fa inh___028_ _x__029_) ::
      (fself_list inh___028_ _x__030_)
  end
let eval_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new eval_list_t) call fa) inh0 subj
let eval_list_fix =
  Fix_eval_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ieval_list.i) ->
             (match sym with | Ieval_list.List -> eval_list_0 f : a)
       })

module Igmap_list =
  (Index2_list)(struct type ('a, 'b) result = unit -> 'a -> 'b end)
module Fix_gmap_list = (FixV)(Igmap_list)
class ['a,'a_2,'extra_list] gmap_list_t _ fa  fself_list =
  object
    inherit  [unit,'a,'a_2,unit,'extra_list,'a_2 list] list_t
    method c_Nil inh___031_ _ = []
    method c_Cons inh___032_ _ _x__033_ _x__034_ = (fa inh___032_ _x__033_) ::
      (fself_list inh___032_ _x__034_)
  end
let gmap_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new gmap_list_t) call fa) inh0 subj
let gmap_list_fix =
  Fix_gmap_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Igmap_list.i) ->
             (match sym with | Igmap_list.List -> gmap_list_0 f : a)
       })

module Ifmt_list =
  (Index_list)(struct type 'a result = Format.formatter -> 'a -> unit end)
module Fix_fmt_list = (FixV)(Ifmt_list)
class ['a,'extra_list] fmt_list_t _   fa  fself_list =
  object
    inherit  [Format.formatter,'a,unit,Format.formatter,'extra_list,unit]
      list_t
    method c_Nil inh___035_ _ = ()
    method c_Cons fmt _ x xs =
      fa fmt x;
      (match xs with [] -> () | _ -> Format.fprintf fmt ";@,@ %a" fself_list xs)
  end
let fmt_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new fmt_list_t) call fa) inh0 subj
let fmt_list_fix =
  Fix_fmt_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ifmt_list.i) ->
             (match sym with | Ifmt_list.List -> fmt_list_0 f : a)
       })

module Ihtml_list =
  (Index_list)(struct type 'a result = unit -> 'a -> HTML.er end)
module Fix_html_list = (FixV)(Ihtml_list)
class ['a,'extra_list] html_list_t { Fix_html_list.call = call }
   fa  fself_list =
  object
    inherit  [unit,'a,HTML.er,unit,'extra_list,HTML.er] list_t
    method c_Nil inh___039_ _ = HTML.ul (HTML.string "[]")
    method c_Cons inh___040_ _ x xs =
      HTML.ul @@ HTML.seq (
        [ HTML.string "list" ] @ List.map (fun x -> HTML.li @@ fa () x) (x::xs)
      )
  end
let html_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new html_list_t) call fa) inh0 subj
let html_list_fix =
  Fix_html_list.fixv
    (fun f ->
       {
         call = fun (type a) ->
           fun (sym : a Ihtml_list.i) ->
             (match sym with | Ihtml_list.List -> html_list_0 f : a)
       })

module Ishow_list =
  (Index_list)(struct type 'a result = unit -> 'a -> string end)
module Fix_show_list = (FixV)(Ishow_list)
class ['a,'extra_list] show_list_t _ fa  fself_list =
  object
    inherit  [unit,'a,string,unit,'extra_list,string] list_t
    method c_Nil inh___043_ _ = ""
    method c_Cons inh___044_ _ x xs =
      (fa () x) ^ (match xs with [] -> "" | _ -> "; " ^ (fself_list () xs))
  end
let show_list_0 call fa inh0 subj =
  transform_gc gcata_list ((new show_list_t) call fa) inh0 subj
let show_list_fix =
  Fix_show_list.fixv
    (fun f ->
       { call = fun (type a) ->
           fun (sym : a Ishow_list.i) ->
             (match sym with | Ishow_list.List -> show_list_0 f : a)
       })

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
  {
    gcata = gcata_list;
    plugins =
      (object
         method eq = eq_list_0 eq_list_fix
         method compare = compare_list_0 compare_list_fix
         method foldr = foldr_list_0 foldr_list_fix
         method foldl = foldl_list_0 foldl_list_fix
         method stateful = stateful_list_0 stateful_list_fix
         method eval = eval_list_0 eval_list_fix
         method gmap fa subj =
           gmap_list_fix.call Igmap_list.List (lift fa) () subj
         method fmt fa fmt s  =
           Format.fprintf fmt "[ @[%a@] ]@," (fmt_list_0 fmt_list_fix fa) s
         method html fa subj =
           html_list_fix.call Ihtml_list.List (lift fa) () subj
         method show fa subj =
           Printf.sprintf "[%a]" (show_list_fix.call Ishow_list.List (lift fa)) subj
       end)
  }


module Lazy =
  struct
    type ('a,'b) t' = ('a,'b) t (* becase Lazy hides type t *)

    include Lazy

    class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] t_t = object
        method virtual t_t : 'inh -> 'a t -> 'syn
      end
    let gcata_t tr inh subj = tr#t_t inh subj
    let gcata_lazy = gcata_t

module type IndexResult_t = sig
  type 'a result

  type 'dummy0 i = T : ('a result -> 'a t result) i
end

module Index_t (S : sig
  type 'a result
end) =
struct
  type 'a result = 'a S.result

  type 'dummy0 i = T : ('a result -> 'a t result) i
end

module type IndexResult2_t = sig
  type ('a, 'b) result

  type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
end

module Index2_t (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

  type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
end

module type IndexResult_fold_t = sig
  type ('a, 'syn) result

  type 'dummy0 i = T : (('a, 'syn) result -> ('a t, 'syn) result) i
end

module Index_fold_t (S : sig
  type ('a, 'syn) result
end) =
struct
  type ('a, 'syn) result = ('a, 'syn) S.result

  type 'dummy0 i = T : (('a, 'syn) result -> ('a t, 'syn) result) i
end

module type IndexResult_stateful_t = sig
  type ('env, 'a, 'b) result

  type 'dummy0 i =
    | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
end

module Index_stateful_t (S : sig
  type ('env, 'a, 'b) result
end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result

  type 'dummy0 i =
    | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
end

module Ishow_t = Index_t (struct
  type 'a result = unit -> 'a -> string
end)

module Fix_show_t = FixV (Ishow_t)

    class ['a, 'self ] show_t_t _ fa _fself =
      object
        inherit [unit, 'a, string, unit, 'self, string ] @t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

let show_t_0 call fa inh0 subj =
  transform_gc gcata_t (new show_t_t call fa) inh0 subj

let show_t_fix =
  Fix_show_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ishow_t.i) ->
            (match sym with Ishow_t.T -> show_t_0 f : a) ) } )

module Ifmt_t = Index_t (struct
  type 'a result = Format.formatter -> 'a -> unit
end)

module Fix_fmt_t = FixV (Ifmt_t)
class ['a, 'extra_t] fmt_t_t _ fa _fself =
  object
    inherit [Format.formatter, 'a, unit, Format.formatter, 'extra_t, unit] t_t

    method t_t inh___004_ subj =
      Format.fprintf inh___004_ "@[(@,%a@,)@]" fa (Lazy.force subj)
  end

let fmt_t_0 call fa inh0 subj =
  transform_gc gcata_t (new fmt_t_t call fa) inh0 subj

let fmt_t_fix =
  Fix_fmt_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifmt_t.i) ->
            (match sym with Ifmt_t.T -> fmt_t_0 f : a) ) } )

module Ihtml_t = Index_t (struct
  type 'a result = unit -> 'a -> HTML.er
end)

module Fix_html_t = FixV (Ihtml_t)

    class ['a, 'self ] html_t_t _ fa _fself =
      object
        inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer ] @t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

let html_t_0 call fa inh0 subj =
  transform_gc gcata_t (new html_t_t call fa) inh0 subj

let html_t_fix =
  Fix_html_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ihtml_t.i) ->
            (match sym with Ihtml_t.T -> html_t_0 f : a) ) } )

module Igmap_t = Index2_t (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_gmap_t = FixV (Igmap_t)

    class ['a, 'sa, 'self ] gmap_t_t _ fa _ =
      object
        inherit [unit, 'a, 'sa, unit, 'self, 'sa t ] @t
        method t_t inh subj = lazy (fa () @@ Lazy.force subj)
      end

let gmap_t_0 call fa inh0 subj =
  transform_gc gcata_t (new gmap_t_t call fa) inh0 subj



let gmap_t_fix =
  Fix_gmap_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Igmap_t.i) ->
            (match sym with Igmap_t.T -> gmap_t_0 f : a) ) } )

module Ieval_t = Index2_t (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_eval_t = FixV (Ieval_t)


    class ['a, 'sa, 'env, 'self ] eval_t_t _ fa _fself =
      object
        inherit ['env, 'a, 'sa, 'env, 'self, 'sa t ] @t
        method t_t env subj = lazy (fa env @@ Lazy.force subj)
      end

let eval_t_0 call fa inh0 subj =
  transform_gc gcata_t (new eval_t_t call fa) inh0 subj

let eval_t_fix =
  Fix_eval_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieval_t.i) ->
            (match sym with Ieval_t.T -> eval_t_0 f : a) ) } )

module Istateful_t = Index_stateful_t (struct
  type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
end)

module Fix_stateful_t = FixV (Istateful_t)


    class ['a, 'sa, 'env, 'self ] stateful_t_t _ fa _fself =
      object
        inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'sa t ] @t
        method t_t env subj =
          let (env1, r) = fa env @@ Lazy.force subj
          in env1, Lazy.from_fun (fun () -> r)
          (* THE SAME AS eval *)
      end
let stateful_t_0 call fa inh0 subj =
  transform_gc gcata_t (new stateful_t_t call fa) inh0 subj

let stateful_t_fix =
  Fix_stateful_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Istateful_t.i) ->
            (match sym with Istateful_t.T -> stateful_t_0 f : a) ) } )

module Icompare_t = Index_t (struct
  type 'a result = 'a -> 'a -> comparison
end)

module Fix_compare_t = FixV (Icompare_t)

    class ['a, 'self ] compare_t_t _ fa _fself =
      object
        inherit ['a, 'a, comparison, 'a t, 'self, comparison ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end
let compare_t_0 call fa inh0 subj =
  transform_gc gcata_t (new compare_t_t call fa) inh0 subj

let compare_t_fix =
  Fix_compare_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Icompare_t.i) ->
            (match sym with Icompare_t.T -> compare_t_0 f : a) ) } )

module Ieq_t = Index_t (struct
  type 'a result = 'a -> 'a -> bool
end)

module Fix_eq_t = FixV (Ieq_t)

    class ['a, 'self ] eq_t_t _ fa _fself =
      object
        inherit ['a, 'a, bool, 'a t, 'self, bool ] @t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

let eq_t_0 call fa inh0 subj =
  transform_gc gcata_t (new eq_t_t call fa) inh0 subj

let eq_t_fix =
  Fix_eq_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieq_t.i) ->
            (match sym with Ieq_t.T -> eq_t_0 f : a) ) } )

module Ifoldl_t = Index_fold_t (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldl_t = FixV (Ifoldl_t)

    class ['a, 'syn, 'self ] foldl_t_t _ fa _fself =
      object
        inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn ] @t
        method t_t inh subj = fa inh @@ Lazy.force subj
      end
let foldl_t_0 call fa inh0 subj =
  transform_gc gcata_t (new foldl_t_t call fa) inh0 subj

let foldl_t_fix =
  Fix_foldl_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldl_t.i) ->
            (match sym with Ifoldl_t.T -> foldl_t_0 f : a) ) } )

module Ifoldr_t = Index_fold_t (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldr_t = FixV (Ifoldr_t)

    class ['a, 'syn, 'self ] foldr_t_t _ fa fself =
      object
        inherit ['a, 'syn, 'self ] @t[foldl] foldl_t_fix fa fself
      end

let foldr_t_0 call fa inh0 subj =
  transform_gc gcata_t (new foldr_t_t call fa) inh0 subj

let foldr_t_fix =
  Fix_foldr_t.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldr_t.i) ->
            (match sym with Ifoldr_t.T -> foldr_t_0 f : a) ) } )

    let t : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #t_t -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      ->
                          'a t -> string;
               html    : ('a -> HTML.viewer) ->
                         'a t -> HTML.viewer;
               fmt     : (Format.formatter -> 'a -> unit) ->
                         Format.formatter -> 'a t -> unit;
               gmap    : ( 'a -> 'b) -> 'a t -> 'b t;

               eval    : ('env -> 'a -> 'b) -> 'env -> 'a t -> 'b t;
               stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a t -> 'env * 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >) t' =
    { gcata   = gcata_lazy;
      plugins = object
        method show fa subj = show_t_fix.call Ishow_t.T (lift fa) () subj

        method fmt = fmt_t_0 fmt_t_fix

        method html fa subj = html_t_fix.call Ihtml_t.T (lift fa) () subj

        method gmap fa subj = gmap_t_fix.call Igmap_t.T (lift fa) () subj

        method eval = eval_t_0 eval_t_fix

        method stateful = stateful_t_0 stateful_t_fix

        method compare = compare_t_0 compare_t_fix

        method eq = eq_t_0 eq_t_fix

        method foldl = foldl_t_0 foldl_t_fix

        method foldr = foldr_t_0 foldr_t_fix
                 end
      }
  end

(* ************************************************************************* *)
type 'a poption = 'a option
type 'a option = 'a poption

class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] option_t =
  object
    method virtual c_None : 'inh -> 'a option -> 'syn

    method virtual c_Some : 'inh -> 'a option -> 'a -> 'syn
  end

let gcata_option tr inh subj =
  match subj with
  | None -> tr#c_None inh subj
  | Some _x__001_ -> tr#c_Some inh subj _x__001_

module type IndexResult_option = sig
  type 'a result

  type 'dummy i = Option : ('a result -> 'a option result) i
end

module Index_option (S : sig
  type 'a result
end) =
struct
  type 'a result = 'a S.result

  type 'dummy i = Option : ('a result -> 'a option result) i
end

module type IndexResult2_option = sig
  type ('a, 'b) result

  type 'dummy i = Option : (('a, 'a2) result -> ('a option, 'a2 option) result) i
end

module Index2_option (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

  type 'dummy i = Option : (('a, 'a2) result -> ('a option, 'a2 option) result) i
end

module type IndexResult_fold_option = sig
  type ('a, 'b) result

  type 'dummy i = Option : (('a, 'a2) result -> ('a option, 'a2) result) i
end

module Index_fold_option (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

  type 'dummy i = Option : (('a, 'a2) result -> ('a option, 'a2) result) i
end

module type IndexResult_stateful_option = sig
  type ('env, 'a, 'b) result

  type 'dummy i =
    | Option
        : (('env, 'a, 'a2) result -> ('env, 'a option, 'a2 option) result) i
end

module Index_stateful_option (S : sig
  type ('env, 'a, 'b) result
end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result

  type 'dummy i =
    | Option
        : (('env, 'a, 'a2) result -> ('env, 'a option, 'a2 option) result) i
end

module Ieq_option = Index_option (struct
  type 'a result = 'a -> 'a -> bool
end)

module Fix_eq_option = FixV (Ieq_option)

class ['a, 'extra_option] eq_option_t _ fa fself_option =
  object
    inherit ['a, 'a, bool, 'a option, 'extra_option, bool] option_t

    method c_None inh___002_ _ =
      match inh___002_ with None -> true | other -> false

    method c_Some inh___003_ _ _x__004_ =
      match inh___003_ with
      | Some _x__005_ -> true && fa _x__005_ _x__004_
      | other -> false
  end

let eq_option_0 call fa inh0 subj =
  transform_gc gcata_option (new eq_option_t call fa) inh0 subj

let eq_option_fix =
  Fix_eq_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieq_option.i) ->
            (match sym with Ieq_option.Option -> eq_option_0 f : a) ) } )

module Icompare_option = Index_option (struct
  type 'a result = 'a -> 'a -> comparison
end)

module Fix_compare_option = FixV (Icompare_option)

class ['a, 'extra_option] compare_option_t _ fa fself_option =
  object
    inherit
      ['a, 'a, comparison, 'a option, 'extra_option, comparison] option_t

    method c_None inh___006_ _ =
      match inh___006_ with
      | None -> EQ
      | other -> compare_vari other None

    method c_Some inh___007_ _ _x__008_ =
      match inh___007_ with
      | Some _x__009_ -> chain_compare EQ (fun () -> fa _x__009_ _x__008_)
      | other -> compare_vari other (Some (Obj.magic ()))
  end

let compare_option_0 call fa inh0 subj =
  transform_gc gcata_option (new compare_option_t call fa) inh0 subj

let compare_option_fix =
  Fix_compare_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Icompare_option.i) ->
            (match sym with Icompare_option.Option -> compare_option_0 f : a)
            ) } )

module Ifoldr_option = Index_fold_option (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldr_option = FixV (Ifoldr_option)

class ['a, 'syn, 'extra_option] foldr_option_t _ fa fself_option =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'extra_option, 'syn] option_t

    method c_None inh___010_ _ = inh___010_

    method c_Some inh___011_ _ _x__012_ = fa inh___011_ _x__012_
  end

let foldr_option_0 call fa inh0 subj =
  transform_gc gcata_option (new foldr_option_t call fa) inh0 subj

let foldr_option_fix =
  Fix_foldr_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldr_option.i) ->
            (match sym with Ifoldr_option.Option -> foldr_option_0 f : a) ) }
  )

module Ifoldl_option = Index_fold_option (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldl_option = FixV (Ifoldl_option)

class ['a, 'syn, 'extra_option] foldl_option_t _ fa fself_option =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'extra_option, 'syn] option_t

    method c_None inh___013_ _ = inh___013_

    method c_Some inh___014_ _ _x__015_ = fa inh___014_ _x__015_
  end

let foldl_option_0 call fa inh0 subj =
  transform_gc gcata_option (new foldl_option_t call fa) inh0 subj

let foldl_option_fix =
  Fix_foldl_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldl_option.i) ->
            (match sym with Ifoldl_option.Option -> foldl_option_0 f : a) ) }
  )

module Istateful_option = Index_stateful_option (struct
  type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
end)

module Fix_stateful_option = FixV (Istateful_option)

class ['a, 'a_2, 'env, 'extra_option] stateful_option_t _ fa fself_option =
  object
    inherit
      ['env, 'a, 'env * 'a_2, 'env, 'extra_option, 'env * 'a_2 option] option_t

    method c_None inh___016_ _ = (inh___016_, None)

    method c_Some inh___017_ _ _x__018_ =
      let env1, _x__018__rez = fa inh___017_ _x__018_ in
      (env1, Some _x__018__rez)
  end

let stateful_option_0 call fa inh0 subj =
  transform_gc gcata_option (new stateful_option_t call fa) inh0 subj

let stateful_option_fix =
  Fix_stateful_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Istateful_option.i) ->
            ( match sym with Istateful_option.Option -> stateful_option_0 f
              : a ) ) } )

module Ieval_option = Index2_option (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_eval_option = FixV (Ieval_option)

class ['a, 'a_2, 'env, 'extra_option] eval_option_t _ fa fself_option =
  object
    inherit ['env, 'a, 'a_2, 'env, 'extra_option, 'a_2 option] option_t

    method c_None inh___019_ _ = None

    method c_Some inh___020_ _ _x__021_ = Some (fa inh___020_ _x__021_)
  end

let eval_option_0 call fa inh0 subj =
  transform_gc gcata_option (new eval_option_t call fa) inh0 subj

let eval_option_fix =
  Fix_eval_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieval_option.i) ->
            (match sym with Ieval_option.Option -> eval_option_0 f : a) ) } )

module Igmap_option = Index2_option (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_gmap_option = FixV (Igmap_option)

class ['a, 'a_2, 'extra_option] gmap_option_t _ fa fself_option =
  object
    inherit [unit, 'a, 'a_2, unit, 'extra_option, 'a_2 option] option_t

    method c_None inh___022_ _ = None

    method c_Some inh___023_ _ _x__024_ = Some (fa inh___023_ _x__024_)
  end

let gmap_option_0 call fa inh0 subj =
  transform_gc gcata_option (new gmap_option_t call fa) inh0 subj

let gmap_option_fix =
  Fix_gmap_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Igmap_option.i) ->
            (match sym with Igmap_option.Option -> gmap_option_0 f : a) ) } )

module Ifmt_option = Index_option (struct
  type 'a result = Format.formatter -> 'a -> unit
end)

module Fix_fmt_option = FixV (Ifmt_option)

class ['a, 'extra_option] fmt_option_t _
  fa fself_option =
  object
    inherit
      [Format.formatter, 'a, unit, Format.formatter, 'extra_option, unit] option_t

    method c_None inh___025_ _ = Format.fprintf inh___025_ "None"

    method c_Some inh___026_ _ _x__027_ =
      Format.fprintf inh___026_ "Some@ @[(@,%a@,)@]" fa _x__027_
  end

let fmt_option_0 call fa inh0 subj =
  transform_gc gcata_option (new fmt_option_t call fa) inh0 subj

let fmt_option_fix =
  Fix_fmt_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifmt_option.i) ->
            (match sym with Ifmt_option.Option -> fmt_option_0 f : a) ) } )

module Ihtml_option = Index_option (struct
  type 'a result = unit -> 'a -> HTML.er
end)

module Fix_html_option = FixV (Ihtml_option)

class ['a, 'extra_option] html_option_t _ fa fself_option =
  object
    inherit [unit, 'a, HTML.er, unit, 'extra_option, HTML.er] option_t

    method c_None inh___028_ _ =
      HTML.ul (HTML.seq (List.cons (HTML.string "None") []))

    method c_Some inh___029_ _ _x__030_ =
      HTML.ul
        (HTML.seq
           (List.cons
              (HTML.li (HTML.seq (List.cons (HTML.string "Some") [])))
              (List.cons
                 (HTML.li (HTML.seq (List.cons (fa () _x__030_) [])))
                 [])))
  end

let html_option_0 call fa inh0 subj =
  transform_gc gcata_option (new html_option_t call fa) inh0 subj

let html_option_fix =
  Fix_html_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ihtml_option.i) ->
            (match sym with Ihtml_option.Option -> html_option_0 f : a) ) } )

module Ishow_option = Index_option (struct
  type 'a result = unit -> 'a -> string
end)

module Fix_show_option = FixV (Ishow_option)

class ['a, 'extra_option] show_option_t _ fa fself_option =
  object
    inherit [unit, 'a, string, unit, 'extra_option, string] option_t

    method c_None inh___031_ _ = "None"

    method c_Some inh___032_ _ _x__033_ =
      Printf.sprintf "Some (%s)" (fa () _x__033_)
  end

let show_option_0 call fa inh0 subj =
  transform_gc gcata_option (new show_option_t call fa) inh0 subj

let show_option_fix =
  Fix_show_option.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ishow_option.i) ->
            (match sym with Ishow_option.Option -> show_option_0 f : a) ) } )

let option: ( ('ia, 'a, 'sa, 'inh, _, 'syn) #option_t -> 'inh -> 'a option -> 'syn,
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
  { gcata= gcata_option
  ; plugins=
      object
        method eq = eq_option_0 eq_option_fix

        method compare = compare_option_0 compare_option_fix

        method foldr = foldr_option_0 foldr_option_fix

        method foldl = foldl_option_0 foldl_option_fix

        method stateful = stateful_option_0 stateful_option_fix

        method eval = eval_option_0 eval_option_fix

        method gmap fa subj =
          gmap_option_fix.call Igmap_option.Option (lift fa) () subj

        method fmt = fmt_option_0 fmt_option_fix

        method html fa subj =
          html_option_fix.call Ihtml_option.Option (lift fa) () subj

        method show fa subj =
          show_option_fix.call Ishow_option.Option (lift fa) () subj
      end }


(* ************************************************************************* *)
(* Antiphantom type *)
type 'a free = 'a
class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] free_t = object
  method virtual c_Free : 'inh -> 'a free -> 'a -> 'syn
end
let gcata_free tr inh subj = tr#c_Free inh subj subj

module type IndexResult_free = sig
  type 'a result

  type 'dummy0 i = Free : ('a result -> 'a free result) i
end

module Index_free (S : sig
  type 'a result
end) =
struct
  type 'a result = 'a S.result

  type 'dummy0 i = Free : ('a result -> 'a free result) i
end

module type IndexResult2_free = sig
  type ('a, 'b) result

  type 'dummy0 i = Free : (('a, 'a2) result -> ('a free, 'a2 free) result) i
end

module Index2_free (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

  type 'dummy0 i = Free : (('a, 'a2) result -> ('a free, 'a2 free) result) i
end

module type IndexResult_fold_free = sig
  type ('a, 'syn) result

  type 'dummy0 i = Free : (('a, 'syn) result -> ('a free, 'syn) result) i
end

module Index_fold_free (S : sig
  type ('a, 'syn) result
end) =
struct
  type ('a, 'syn) result = ('a, 'syn) S.result

  type 'dummy0 i = Free : (('a, 'syn) result -> ('a free, 'syn) result) i
end

module type IndexResult_stateful_free = sig
  type ('env, 'a, 'b) result

  type 'dummy0 i =
    | Free : (('env, 'a, 'a2) result -> ('env, 'a free, 'a2 free) result) i
end

module Index_stateful_free (S : sig
  type ('env, 'a, 'b) result
end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result

  type 'dummy0 i =
    | Free : (('env, 'a, 'a2) result -> ('env, 'a free, 'a2 free) result) i
end


module Ishow_free = Index_free (struct
  type 'a result = unit -> 'a -> string
end)

module Fix_show_free = FixV (Ishow_free)

class ['a, 'extra_free] show_free_t _ fa  fself_free =
  object
    inherit [unit, 'a, string, unit, 'extra_free, string] free_t

    method c_Free inh___002_ _ _x__003_ =
      Printf.sprintf "(%s)" (fa () _x__003_)
  end

let show_free_0 call fa inh0 subj =
  transform_gc gcata_free (new show_free_t call fa) inh0 subj

let show_free_fix =
  Fix_show_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ishow_free.i) ->
            (match sym with Ishow_free.Free -> show_free_0 f : a) ) } )

module Ifmt_free = Index_free (struct
  type 'a result = Format.formatter -> 'a -> unit
end)

module Fix_fmt_free = FixV (Ifmt_free)

class ['a, 'extra_free] fmt_free_t _ fa
  fself_free =
  object
    inherit
      [Format.formatter, 'a, unit, Format.formatter, 'extra_free, unit] free_t

    method c_Free inh___004_ _ _x__005_ =
      Format.fprintf inh___004_ "@[(@,%a@,)@]" fa _x__005_
  end

let fmt_free_0 call fa inh0 subj =
  transform_gc gcata_free (new fmt_free_t call fa) inh0 subj

let fmt_free_fix =
  Fix_fmt_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifmt_free.i) ->
            (match sym with Ifmt_free.Free -> fmt_free_0 f : a) ) } )

module Ihtml_free = Index_free (struct
  type 'a result = unit -> 'a -> HTML.er
end)

module Fix_html_free = FixV (Ihtml_free)

class ['a, 'extra_free] html_free_t _ fa
  fself_free =
  object
    inherit [unit, 'a, HTML.er, unit, 'extra_free, HTML.er] free_t

    method c_Free inh___006_ _ _x__007_ =
      HTML.ul
        (HTML.seq
           (List.cons
              (HTML.li (HTML.seq (List.cons (HTML.string "Free") [])))
              (List.cons
                 (HTML.li (HTML.seq (List.cons (fa () _x__007_) [])))
                 [])))
  end

let html_free_0 call fa inh0 subj =
  transform_gc gcata_free (new html_free_t call fa) inh0 subj

let html_free_fix =
  Fix_html_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ihtml_free.i) ->
            (match sym with Ihtml_free.Free -> html_free_0 f : a) ) } )

module Igmap_free = Index2_free (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_gmap_free = FixV (Igmap_free)

class ['a, 'a_2, 'extra_free] gmap_free_t _ fa fself_free =
  object
    inherit [unit, 'a, 'a_2, unit, 'extra_free, 'a_2 free] free_t

    method c_Free inh___008_ _ _x__009_ = (fa inh___008_ _x__009_)
  end

let gmap_free_0 call fa inh0 subj =
  transform_gc gcata_free (new gmap_free_t call fa) inh0 subj

let gmap_free_fix =
  Fix_gmap_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Igmap_free.i) ->
            (match sym with Igmap_free.Free -> gmap_free_0 f : a) ) } )

module Ieval_free = Index2_free (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_eval_free = FixV (Ieval_free)

class ['a, 'a_2, 'env, 'extra_free] eval_free_t _ fa fself_free =
  object
    inherit ['env, 'a, 'a_2, 'env, 'extra_free, 'a_2 free] free_t

    method c_Free inh___010_ _ _x__011_ = (fa inh___010_ _x__011_)
  end

let eval_free_0 call fa inh0 subj =
  transform_gc gcata_free (new eval_free_t call fa) inh0 subj

let eval_free_fix =
  Fix_eval_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieval_free.i) ->
            (match sym with Ieval_free.Free -> eval_free_0 f : a) ) } )

module Istateful_free = Index_stateful_free (struct
  type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
end)

module Fix_stateful_free = FixV (Istateful_free)

class ['a, 'a_2, 'env, 'extra_free] stateful_free_t _ fa fself_free =
  object
    inherit ['env, 'a, 'env * 'a_2, 'env, 'extra_free, 'env * 'a_2 free] free_t

    method c_Free inh___012_ _ _x__013_ =
      let env1, _x__013__rez = fa inh___012_ _x__013_ in
      (env1,  _x__013__rez)
  end

let stateful_free_0 call fa inh0 subj =
  transform_gc gcata_free (new stateful_free_t call fa) inh0 subj

let stateful_free_fix =
  Fix_stateful_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Istateful_free.i) ->
            (match sym with Istateful_free.Free -> stateful_free_0 f : a) ) }
  )

module Icompare_free = Index_free (struct
  type 'a result = 'a -> 'a -> comparison
end)

module Fix_compare_free = FixV (Icompare_free)

class ['a, 'extra_free] compare_free_t _  fa fself_free =
  object
    inherit ['a, 'a, comparison, 'a free, 'extra_free, comparison] free_t

    method c_Free inh___014_ _ _x__015_ =
      match inh___014_ with
      | _x__016_ -> chain_compare EQ (fun () -> fa _x__016_ _x__015_)
  end

let compare_free_0 call fa inh0 subj =
  transform_gc gcata_free (new compare_free_t call fa) inh0 subj

let compare_free_fix =
  Fix_compare_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Icompare_free.i) ->
            (match sym with Icompare_free.Free -> compare_free_0 f : a) ) } )

module Ieq_free = Index_free (struct
  type 'a result = 'a -> 'a -> bool
end)

module Fix_eq_free = FixV (Ieq_free)

class ['a, 'extra_free] eq_free_t _ fa
  fself_free =
  object
    inherit ['a, 'a, bool, 'a free, 'extra_free, bool] free_t

    method c_Free inh___017_ _ _x__018_ =
      match inh___017_ with
      | _x__019_ -> true && fa _x__019_ _x__018_
  end

let eq_free_0 call fa inh0 subj =
  transform_gc gcata_free (new eq_free_t call fa) inh0 subj

let eq_free_fix =
  Fix_eq_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieq_free.i) ->
            (match sym with Ieq_free.Free -> eq_free_0 f : a) ) } )

module Ifoldl_free = Index_fold_free (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldl_free = FixV (Ifoldl_free)

class ['a, 'syn, 'extra_free] foldl_free_t _  fa fself_free =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'extra_free, 'syn] free_t

    method c_Free inh___020_ _ _x__021_ = fa inh___020_ _x__021_
  end

let foldl_free_0 call fa inh0 subj =
  transform_gc gcata_free (new foldl_free_t call fa) inh0 subj

let foldl_free_fix =
  Fix_foldl_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldl_free.i) ->
            (match sym with Ifoldl_free.Free -> foldl_free_0 f : a) ) } )

module Ifoldr_free = Index_fold_free (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldr_free = FixV (Ifoldr_free)

class ['a, 'syn, 'extra_free] foldr_free_t _ fa fself_free =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'extra_free, 'syn] free_t

    method c_Free inh___022_ _ _x__023_ = fa inh___022_ _x__023_
  end

let foldr_free_0 call fa inh0 subj =
  transform_gc gcata_free (new foldr_free_t call fa) inh0 subj

let foldr_free_fix =
  Fix_foldr_free.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldr_free.i) ->
            (match sym with Ifoldr_free.Free -> foldr_free_0 f : a) ) } )

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
        method show fa subj =
          show_free_fix.call Ishow_free.Free (lift fa) () subj

        method fmt = fmt_free_0 fmt_free_fix

        method html fa subj =
          html_free_fix.call Ihtml_free.Free (lift fa) () subj

        method gmap fa subj =
          gmap_free_fix.call Igmap_free.Free (lift fa) () subj

        method eval = eval_free_0 eval_free_fix

        method stateful = stateful_free_0 stateful_free_fix

        method compare = compare_free_0 compare_free_fix

        method eq = eq_free_0 eq_free_fix

        method foldl = foldl_free_0 foldl_free_fix

        method foldr = foldr_free_0 foldr_free_fix
  end
  }

(* *************************************************************************** *)
(* Pairs and other stuff without explicit structure *)
type ('a, 'b) pair = 'a * 'b

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] pair_t =
  object
    method virtual c_Pair : 'inh -> ('a, 'b) pair -> 'a -> 'b -> 'syn
  end

let gcata_pair tr inh subj =
  match subj with  (_x__001_, _x__002_) -> tr#c_Pair inh subj _x__001_ _x__002_

module type IndexResult_pair = sig
  type 'a result

type 'dummy1 i = Pair : ('a result -> 'b result -> ('a, 'b) pair result) i
end

module Index_pair (S : sig
  type 'a result
end) =
struct
  type 'a result = 'a S.result

type 'dummy1 i = Pair : ('a result -> 'b result -> ('a, 'b) pair result) i
end

module type IndexResult2_pair = sig
  type ('a, 'b) result

type 'dummy1 i =
    | Pair
        : (   ('a, 'a2) result
           -> ('b, 'b2) result
           -> (('a, 'b) pair, ('a2, 'b2) pair) result)
          i
end

module Index2_pair (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

type 'dummy1 i =
    | Pair
        : (   ('a, 'a2) result
           -> ('b, 'b2) result
           -> (('a, 'b) pair, ('a2, 'b2) pair) result)
          i
end

module type IndexResult_fold_pair = sig
  type ('a, 'b) result

  type 'dummy1 i =
    | Pair
        : (   ('a, 'a2) result
           -> ('b, 'b2) result
           -> (('a, 'b) pair, 'a2) result)
          i
end

module Index_fold_pair (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

type 'dummy1 i =
    | Pair
        : (   ('a, 's) result
           -> ('b, 's) result
           -> (('a, 'b) pair, 's) result)
          i
end

module type IndexResult_stateful_pair = sig
  type ('env, 'a, 'b) result

type 'dummy1 i =
    | Pair
        : (   ('env, 'a, 'a2) result
           -> ('env, 'b, 'b2) result
           -> ('env, ('a, 'b) pair, ('a2, 'b2) pair) result)
          i
end

module Index_stateful_pair (S : sig
  type ('env, 'a, 'b) result
end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result

type 'dummy1 i =
    | Pair
        : (   ('env, 'a, 'a2) result
           -> ('env, 'b, 'b2) result
           -> ('env, ('a, 'b) pair, ('a2, 'b2) pair) result)
          i
end

module Ieq_pair = Index_pair (struct
  type 'a result = 'a -> 'a -> bool
end)

module Fix_eq_pair = FixV (Ieq_pair)

class ['a, 'b, 'extra_pair] eq_pair_t _  fa fb fself_pair =
  object
    inherit
      ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) pair, 'extra_pair, bool] pair_t

    method c_Pair inh___003_ _ _x__004_ _x__005_ =
      match inh___003_ with
      |  (_x__006_, _x__007_) ->
          (true && fa _x__006_ _x__004_) && fb _x__007_ _x__005_
  end

let eq_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new eq_pair_t call fa fb) inh0 subj

let eq_pair_fix =
  Fix_eq_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieq_pair.i) ->
            (match sym with Ieq_pair.Pair -> eq_pair_0 f : a) ) } )

module Icompare_pair = Index_pair (struct
  type 'a result = 'a -> 'a -> comparison
end)

module Fix_compare_pair = FixV (Icompare_pair)

class ['a, 'b, 'extra_pair] compare_pair_t _  fa fb fself_pair =
  object
    inherit
      [ 'a
      , 'a
      , comparison
      , 'b
      , 'b
      , comparison
      , ('a, 'b) pair
      , 'extra_pair
      , comparison ]
      pair_t

    method c_Pair inh___008_ _ _x__009_ _x__010_ =
      match inh___008_ with
      |  (_x__011_, _x__012_) ->
          chain_compare
            (chain_compare EQ (fun () -> fa _x__011_ _x__009_))
            (fun () -> fb _x__012_ _x__010_)
  end

let compare_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new compare_pair_t call fa fb) inh0 subj

let compare_pair_fix =
  Fix_compare_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Icompare_pair.i) ->
            (match sym with Icompare_pair.Pair -> compare_pair_0 f : a)
            ) } )

module Ifoldr_pair = Index_fold_pair (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldr_pair = FixV (Ifoldr_pair)

class ['a, 'b, 'syn, 'extra_pair] foldr_pair_t _  fa fb fself_pair =
  object
    inherit
      ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'extra_pair, 'syn] pair_t

    method c_Pair inh___013_ _ _x__014_ _x__015_ =
      fa (fb inh___013_ _x__015_) _x__014_
  end

let foldr_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new foldr_pair_t call fa fb) inh0 subj

let foldr_pair_fix =
  Fix_foldr_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldr_pair.i) ->
            (match sym with Ifoldr_pair.Pair -> foldr_pair_0 f : a) ) }
  )

module Ifoldl_pair = Index_fold_pair (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldl_pair = FixV (Ifoldl_pair)

class ['a, 'b, 'syn, 'extra_pair] foldl_pair_t _  fa fb fself_pair =
  object
    inherit
      ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'extra_pair, 'syn] pair_t

    method c_Pair inh___016_ _ _x__017_ _x__018_ =
      fb (fa inh___016_ _x__017_) _x__018_
  end

let foldl_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new foldl_pair_t call fa fb) inh0 subj

let foldl_pair_fix =
  Fix_foldl_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldl_pair.i) ->
            (match sym with Ifoldl_pair.Pair -> foldl_pair_0 f : a) ) }
  )

module Istateful_pair = Index_stateful_pair (struct
  type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
end)

module Fix_stateful_pair = FixV (Istateful_pair)

class ['a, 'a_2, 'b, 'b_2, 'env, 'extra_pair] stateful_pair_t _  fa fb fself_pair =
  object
    inherit
      [ 'env
      , 'a
      , 'env * 'a_2
      , 'env
      , 'b
      , 'env * 'b_2
      , 'env
      , 'extra_pair
      , 'env * ('a_2, 'b_2) pair ]
      pair_t

    method c_Pair inh___019_ _ _x__020_ _x__021_ =
      let env1, _x__020__rez = fa inh___019_ _x__020_ in
      let env2, _x__021__rez = fb env1 _x__021_ in
      (env2,  (_x__020__rez, _x__021__rez))
  end

let stateful_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new stateful_pair_t call fa fb) inh0 subj

let stateful_pair_fix =
  Fix_stateful_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Istateful_pair.i) ->
            ( match sym with Istateful_pair.Pair -> stateful_pair_0 f
              : a ) ) } )

module Ieval_pair = Index2_pair (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_eval_pair = FixV (Ieval_pair)

class ['a, 'a_2, 'b, 'b_2, 'env, 'extra_pair] eval_pair_t _  fa fb fself_pair =
  object
    inherit
      [ 'env
      , 'a
      , 'a_2
      , 'env
      , 'b
      , 'b_2
      , 'env
      , 'extra_pair
      , ('a_2, 'b_2) pair ]
      pair_t

    method c_Pair inh___022_ _ _x__023_ _x__024_ =
       (fa inh___022_ _x__023_, fb inh___022_ _x__024_)
  end

let eval_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new eval_pair_t call fa fb) inh0 subj

let eval_pair_fix =
  Fix_eval_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieval_pair.i) ->
            (match sym with Ieval_pair.Pair -> eval_pair_0 f : a) ) } )

module Igmap_pair = Index2_pair (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_gmap_pair = FixV (Igmap_pair)

class ['a, 'a_2, 'b, 'b_2, 'extra_pair] gmap_pair_t _  fa fb fself_pair =
  object
    inherit
      [ unit
      , 'a
      , 'a_2
      , unit
      , 'b
      , 'b_2
      , unit
      , 'extra_pair
      , ('a_2, 'b_2) pair ]
      pair_t

    method c_Pair inh___025_ _ _x__026_ _x__027_ =
       (fa inh___025_ _x__026_, fb inh___025_ _x__027_)
  end

let gmap_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new gmap_pair_t call fa fb) inh0 subj

let gmap_pair_fix =
  Fix_gmap_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Igmap_pair.i) ->
            (match sym with Igmap_pair.Pair -> gmap_pair_0 f : a) ) } )

module Ifmt_pair = Index_pair (struct
  type 'a result = Format.formatter -> 'a -> unit
end)

module Fix_fmt_pair = FixV (Ifmt_pair)

class ['a, 'b, 'extra_pair] fmt_pair_t _  fa fb fself_pair =
  object
    inherit
      [ Format.formatter
      , 'a
      , unit
      , Format.formatter
      , 'b
      , unit
      , Format.formatter
      , 'extra_pair
      , unit ]
      pair_t

    method c_Pair inh___028_ _ _x__029_ _x__030_ =
      Format.fprintf inh___028_ "@[(@,%a,@,@ %a@,)@]" fa _x__029_ fb
        _x__030_
  end

let fmt_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new fmt_pair_t call fa fb) inh0 subj

let fmt_pair_fix =
  Fix_fmt_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifmt_pair.i) ->
            (match sym with Ifmt_pair.Pair -> fmt_pair_0 f : a) ) } )

module Ihtml_pair = Index_pair (struct
  type 'a result = unit -> 'a -> HTML.er
end)

module Fix_html_pair = FixV (Ihtml_pair)

class ['a, 'b, 'extra_pair] html_pair_t _  fa fb fself_pair =
  object
    inherit
      [unit, 'a, HTML.er, unit, 'b, HTML.er, unit, 'extra_pair, HTML.er] pair_t

    method c_Pair inh___031_ _ _x__032_ _x__033_ =
      HTML.ul
        (HTML.seq
           (List.cons
              (HTML.li (HTML.seq (List.cons (HTML.string "") [])))
              (List.cons
                 (HTML.li (HTML.seq (List.cons (fa () _x__032_) [])))
                 (List.cons
                    (HTML.li (HTML.seq (List.cons (fb () _x__033_) [])))
                    []))))
  end

let html_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new html_pair_t call fa fb) inh0 subj

let html_pair_fix =
  Fix_html_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ihtml_pair.i) ->
            (match sym with Ihtml_pair.Pair -> html_pair_0 f : a) ) } )

module Ishow_pair = Index_pair (struct
  type 'a result = unit -> 'a -> string
end)

module Fix_show_pair = FixV (Ishow_pair)

class ['a, 'b, 'extra_pair] show_pair_t _  fa fb fself_pair =
  object
    inherit
      [unit, 'a, string, unit, 'b, string, unit, 'extra_pair, string] pair_t

    method c_Pair inh___034_ _ _x__035_ _x__036_ =
      Printf.sprintf "(%s, %s)" (fa () _x__035_) (fb () _x__036_)
  end

let show_pair_0 call fa fb inh0 subj =
  transform_gc gcata_pair (new show_pair_t call fa fb) inh0 subj

let show_pair_fix =
  Fix_show_pair.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ishow_pair.i) ->
            (match sym with Ishow_pair.Pair -> show_pair_0 f : a) ) } )

let pair :
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
  { gcata= gcata_pair
  ; plugins=
      object
        method eq = eq_pair_0 eq_pair_fix

        method compare = compare_pair_0 compare_pair_fix

        method foldr = foldr_pair_0 foldr_pair_fix

        method foldl = foldl_pair_0 foldl_pair_fix

        method stateful = stateful_pair_0 stateful_pair_fix

        method eval = eval_pair_0 eval_pair_fix

        method gmap fa fb subj =
          gmap_pair_fix.call Igmap_pair.Pair (lift fa) (lift fb) ()
            subj

        method fmt = fmt_pair_0 fmt_pair_fix

        method html fa fb subj =
          html_pair_fix.call Ihtml_pair.Pair (lift fa) (lift fb) ()
            subj

        method show fa fb subj =
          show_pair_fix.call Ishow_pair.Pair (lift fa) (lift fb) ()
            subj
      end }

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] tuple2_t = object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] pair_t
end
let gcata_tuple2 = gcata_pair
let tuple2 = pair

(* Just aliases *)
class ['a, 'b, 'self] show_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'self] show_pair_t m fa fb fself
end
let show_tuple2_fix = show_pair_fix
class ['a, 'b, 'self] fmt_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'self] fmt_pair_t m fa fb fself
end
let fmt_tuple2_fix = fmt_pair_fix
class ['a, 'b, 'self] html_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'self] html_pair_t m fa fb fself
end
let html_tuple2_fix = html_pair_fix
class ['a, 'a2, 'b, 'b2, 'self] gmap_tuple2_t m fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'self] gmap_pair_t m fa fb fself
end
let gmap_tuple2_fix = gmap_pair_fix
class ['a, 'a2, 'b, 'b2, 'env, 'self] eval_tuple2_t m fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'env, 'self] eval_pair_t m fa fb fself
end
let eval_tuple2_fix = eval_pair_fix
class ['a, 'a2, 'b, 'b2, 'env, 'self] stateful_tuple2_t m fa fb fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'env, 'self] stateful_pair_t m fa fb fself
end
let stateful_tuple2_fix = stateful_pair_fix
class ['a, 'b, 'self] compare_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'self] compare_pair_t m fa fb fself
end
let compare_tuple2_fix = compare_pair_fix
class ['a, 'b, 'self] eq_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'self] eq_pair_t m fa fb fself
end
let eq_tuple2_fix = eq_pair_fix
class ['a, 'b, 'syn, 'self] foldl_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'syn, 'self] foldl_pair_t m fa fb fself
end
let foldl_tuple2_fix = foldl_pair_fix
class ['a, 'b, 'syn, 'self] foldr_tuple2_t m fa fb fself = object
  inherit [ 'a, 'b, 'syn, 'self] foldr_pair_t m fa fb fself
end
let foldr_tuple2_fix = foldr_pair_fix

(*******************************************************************************)
(* Tuples of size 3 *)
type ('a,'b,'c) triple = 'a * 'b * 'c

class virtual
  ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'ic, 'c, 'sc, 'inh, 'extra, 'syn] triple_t =
  object
    method virtual c_Triple
        : 'inh -> ('a, 'b, 'c) triple -> 'a -> 'b -> 'c -> 'syn
  end
let gcata_triple tr inh ((a,b,c) as subj) = tr#c_Triple inh subj a b c

module type IndexResult_triple = sig
  type 'a result

  type 'dummy0 i =
    | Triple
        : ('a result -> 'b result -> 'c result -> ('a, 'b, 'c) triple result) i
end

module Index_triple (S : sig
  type 'a result
end) =
struct
  type 'a result = 'a S.result

  type 'dummy0 i =
    | Triple
        : ('a result -> 'b result -> 'c result -> ('a, 'b, 'c) triple result) i
end

module type IndexResult2_triple = sig
  type ('a, 'b) result

  type 'dummy0 i =
    | Triple
        : (   ('a, 'a2) result
           -> ('b, 'b2) result
           -> ('c, 'c2) result
           -> (('a, 'b, 'c) triple, ('a2, 'b2, 'c2) triple) result)
          i
end

module Index2_triple (S : sig
  type ('a, 'b) result
end) =
struct
  type ('a, 'b) result = ('a, 'b) S.result

  type 'dummy0 i =
    | Triple
        : (   ('a, 'a2) result
           -> ('b, 'b2) result
           -> ('c, 'c2) result
           -> (('a, 'b, 'c) triple, ('a2, 'b2, 'c2) triple) result)
          i
end

module type IndexResult_fold_triple = sig
  type ('a, 'syn) result

  type 'dummy0 i =
    | Triple
        : (   ('a, 'syn) result
           -> ('b, 'syn) result
           -> ('c, 'syn) result
           -> (('a, 'b, 'c) triple, 'syn) result)
          i
end

module Index_fold_triple (S : sig
  type ('a, 'syn) result
end) =
struct
  type ('a, 'syn) result = ('a, 'syn) S.result

  type 'dummy0 i =
    | Triple
        : (   ('a, 'syn) result
           -> ('b, 'syn) result
           -> ('c, 'syn) result
           -> (('a, 'b, 'c) triple, 'syn) result)
          i
end

module type IndexResult_stateful_triple = sig
  type ('env, 'a, 'b) result

  type 'dummy0 i =
    | Triple
        : (   ('env, 'a, 'a2) result
           -> ('env, 'b, 'b2) result
           -> ('env, 'c, 'c2) result
           -> ('env, ('a, 'b, 'c) triple, ('a2, 'b2, 'c2) triple) result)
          i
end

module Index_stateful_triple (S : sig
  type ('env, 'a, 'b) result
end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result

  type 'dummy0 i =
    | Triple
        : (   ('env, 'a, 'a2) result
           -> ('env, 'b, 'b2) result
           -> ('env, 'c, 'c2) result
           -> ('env, ('a, 'b, 'c) triple, ('a2, 'b2, 'c2) triple) result)
          i
end

module Ishow_triple = Index_triple (struct
  type 'a result = unit -> 'a -> string
end)

module Fix_show_triple = FixV (Ishow_triple)

class ['a, 'b, 'c, 'extra_triple] show_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ unit
      , 'a
      , string
      , unit
      , 'b
      , string
      , unit
      , 'c
      , string
      , unit
      , 'extra_triple
      , string ]
      triple_t

    method c_Triple inh___004_ _ _x__005_ _x__006_ _x__007_ =
      Printf.sprintf "(%s, %s, %s)" (fa () _x__005_) (fb () _x__006_)
        (fc () _x__007_)
  end

let show_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new show_triple_t call fa fb fc) inh0 subj

let show_triple_fix =
  Fix_show_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ishow_triple.i) ->
            (match sym with Ishow_triple.Triple -> show_triple_0 f : a) ) } )

module Ifmt_triple = Index_triple (struct
  type 'a result = Format.formatter -> 'a -> unit
end)

module Fix_fmt_triple = FixV (Ifmt_triple)

class ['a, 'b, 'c, 'extra_triple] fmt_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ Format.formatter
      , 'a
      , unit
      , Format.formatter
      , 'b
      , unit
      , Format.formatter
      , 'c
      , unit
      , Format.formatter
      , 'extra_triple
      , unit ]
      triple_t

    method c_Triple inh___008_ _ _x__009_ _x__010_ _x__011_ =
      Format.fprintf inh___008_ "Triple@ @[(@,%a,@,@ %a,@,@ %a@,)@]" fa
        _x__009_ fb _x__010_ fc _x__011_
  end

let fmt_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new fmt_triple_t call fa fb fc) inh0 subj

let fmt_triple_fix =
  Fix_fmt_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifmt_triple.i) ->
            (match sym with Ifmt_triple.Triple -> fmt_triple_0 f : a) ) } )

module Ihtml_triple = Index_triple (struct
  type 'a result = unit -> 'a -> HTML.er
end)

module Fix_html_triple = FixV (Ihtml_triple)

class ['a, 'b, 'c, 'extra_triple] html_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ unit
      , 'a
      , HTML.er
      , unit
      , 'b
      , HTML.er
      , unit
      , 'c
      , HTML.er
      , unit
      , 'extra_triple
      , HTML.er ]
      triple_t

    method c_Triple inh___012_ _ _x__013_ _x__014_ _x__015_ =
      HTML.ul
        (HTML.seq
           (List.cons
              (HTML.li (HTML.seq (List.cons (HTML.string "Triple") [])))
              (List.cons
                 (HTML.li (HTML.seq (List.cons (fa () _x__013_) [])))
                 (List.cons
                    (HTML.li (HTML.seq (List.cons (fb () _x__014_) [])))
                    (List.cons
                       (HTML.li (HTML.seq (List.cons (fc () _x__015_) [])))
                       [])))))
  end

let html_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new html_triple_t call fa fb fc) inh0 subj

let html_triple_fix =
  Fix_html_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ihtml_triple.i) ->
            (match sym with Ihtml_triple.Triple -> html_triple_0 f : a) ) } )

module Igmap_triple = Index2_triple (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_gmap_triple = FixV (Igmap_triple)

class ['a, 'a_2, 'b, 'b_2, 'c, 'c_2, 'extra_triple] gmap_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ unit
      , 'a
      , 'a_2
      , unit
      , 'b
      , 'b_2
      , unit
      , 'c
      , 'c_2
      , unit
      , 'extra_triple
      , ('a_2, 'b_2, 'c_2) triple ]
      triple_t

    method c_Triple inh___016_ _ _x__017_ _x__018_ _x__019_ =
        (fa inh___016_ _x__017_, fb inh___016_ _x__018_, fc inh___016_ _x__019_)
  end

let gmap_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new gmap_triple_t call fa fb fc) inh0 subj

let gmap_triple_fix =
  Fix_gmap_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Igmap_triple.i) ->
            (match sym with Igmap_triple.Triple -> gmap_triple_0 f : a) ) } )

module Ieval_triple = Index2_triple (struct
  type ('a, 'b) result = unit -> 'a -> 'b
end)

module Fix_eval_triple = FixV (Ieval_triple)

class ['a, 'a_2, 'b, 'b_2, 'c, 'c_2, 'env, 'extra_triple] eval_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ 'env
      , 'a
      , 'a_2
      , 'env
      , 'b
      , 'b_2
      , 'env
      , 'c
      , 'c_2
      , 'env
      , 'extra_triple
      , ('a_2, 'b_2, 'c_2) triple ]
      triple_t

    method c_Triple inh___020_ _ _x__021_ _x__022_ _x__023_ =
        (fa inh___020_ _x__021_, fb inh___020_ _x__022_, fc inh___020_ _x__023_)
  end

let eval_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new eval_triple_t call fa fb fc) inh0 subj

let eval_triple_fix =
  Fix_eval_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieval_triple.i) ->
            (match sym with Ieval_triple.Triple -> eval_triple_0 f : a) ) } )

module Istateful_triple = Index_stateful_triple (struct
  type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
end)

module Fix_stateful_triple = FixV (Istateful_triple)

class ['a, 'a_2, 'b, 'b_2, 'c, 'c_2, 'env, 'extra_triple] stateful_triple_t _ fa fb fc fself_triple =
  object
    inherit
      [ 'env
      , 'a
      , 'env * 'a_2
      , 'env
      , 'b
      , 'env * 'b_2
      , 'env
      , 'c
      , 'env * 'c_2
      , 'env
      , 'extra_triple
      , 'env * ('a_2, 'b_2, 'c_2) triple ]
      triple_t

    method c_Triple inh___024_ _ _x__025_ _x__026_ _x__027_ =
      let env1, _x__025__rez = fa inh___024_ _x__025_ in
      let env2, _x__026__rez = fb env1 _x__026_ in
      let env3, _x__027__rez = fc env2 _x__027_ in
      (env3, (_x__025__rez, _x__026__rez, _x__027__rez))
  end

let stateful_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new stateful_triple_t call fa fb fc) inh0 subj

let stateful_triple_fix =
  Fix_stateful_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Istateful_triple.i) ->
            ( match sym with Istateful_triple.Triple -> stateful_triple_0 f
              : a ) ) } )

module Icompare_triple = Index_triple (struct
  type 'a result = 'a -> 'a -> comparison
end)

module Fix_compare_triple = FixV (Icompare_triple)

class ['a, 'b, 'c, 'extra_triple] compare_triple_t _  fa fb fc fself_triple =
  object
    inherit
      [ 'a
      , 'a
      , comparison
      , 'b
      , 'b
      , comparison
      , 'c
      , 'c
      , comparison
      , ('a, 'b, 'c) triple
      , 'extra_triple
      , comparison ]
      triple_t

    method c_Triple inh___028_ _ _x__029_ _x__030_ _x__031_ =
      match inh___028_ with
      |  (_x__032_, _x__033_, _x__034_) ->
          chain_compare
            (chain_compare
               (chain_compare EQ (fun () -> fa _x__032_ _x__029_))
               (fun () -> fb _x__033_ _x__030_))
            (fun () -> fc _x__034_ _x__031_)
  end

let compare_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new compare_triple_t call fa fb fc) inh0 subj

let compare_triple_fix =
  Fix_compare_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Icompare_triple.i) ->
            (match sym with Icompare_triple.Triple -> compare_triple_0 f : a)
            ) } )

module Ieq_triple = Index_triple (struct
  type 'a result = 'a -> 'a -> bool
end)

module Fix_eq_triple = FixV (Ieq_triple)

class ['a, 'b, 'c, 'extra_triple] eq_triple_t _  fa fb fc fself_triple =
  object
    inherit
      [ 'a
      , 'a
      , bool
      , 'b
      , 'b
      , bool
      , 'c
      , 'c
      , bool
      , ('a, 'b, 'c) triple
      , 'extra_triple
      , bool ]
      triple_t

    method c_Triple inh___035_ _ _x__036_ _x__037_ _x__038_ =
      match inh___035_ with
      | (_x__039_, _x__040_, _x__041_) ->
          ((true && fa _x__039_ _x__036_) && fb _x__040_ _x__037_)
          && fc _x__041_ _x__038_
  end

let eq_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new eq_triple_t call fa fb fc) inh0 subj

let eq_triple_fix =
  Fix_eq_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ieq_triple.i) ->
            (match sym with Ieq_triple.Triple -> eq_triple_0 f : a) ) } )

module Ifoldl_triple = Index_fold_triple (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldl_triple = FixV (Ifoldl_triple)

class ['a, 'b, 'c, 'syn, 'extra_triple] foldl_triple_t _  fa fb fc fself_triple =
  object
    inherit
      [ 'syn
      , 'a
      , 'syn
      , 'syn
      , 'b
      , 'syn
      , 'syn
      , 'c
      , 'syn
      , 'syn
      , 'extra_triple
      , 'syn ]
      triple_t

    method c_Triple inh___042_ _ _x__043_ _x__044_ _x__045_ =
      fc (fb (fa inh___042_ _x__043_) _x__044_) _x__045_
  end

let foldl_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new foldl_triple_t call fa fb fc) inh0 subj

let foldl_triple_fix =
  Fix_foldl_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldl_triple.i) ->
            (match sym with Ifoldl_triple.Triple -> foldl_triple_0 f : a) ) }
  )

module Ifoldr_triple = Index_fold_triple (struct
  type ('a, 'b) result = 'b -> 'a -> 'b
end)

module Fix_foldr_triple = FixV (Ifoldr_triple)

class ['a, 'b, 'c, 'syn, 'extra_triple] foldr_triple_t _  fa fb fc fself_triple =
  object
    inherit
      [ 'syn
      , 'a
      , 'syn
      , 'syn
      , 'b
      , 'syn
      , 'syn
      , 'c
      , 'syn
      , 'syn
      , 'extra_triple
      , 'syn ]
      triple_t

    method c_Triple inh___046_ _ _x__047_ _x__048_ _x__049_ =
      fa (fb (fc inh___046_ _x__049_) _x__048_) _x__047_
  end

let foldr_triple_0 call fa fb fc inh0 subj =
  transform_gc gcata_triple (new foldr_triple_t call fa fb fc) inh0 subj

let foldr_triple_fix =
  Fix_foldr_triple.fixv (fun f ->
      { call=
          (fun (type a) (sym : a Ifoldr_triple.i) ->
            (match sym with Ifoldr_triple.Triple -> foldr_triple_0 f : a) ) }
  )

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
   plugins = object
        method show fa fb fc subj =
          show_triple_fix.call Ishow_triple.Triple (lift fa) (lift fb)
            (lift fc) () subj

        method fmt = fmt_triple_0 fmt_triple_fix

        method html fa fb fc subj =
          html_triple_fix.call Ihtml_triple.Triple (lift fa) (lift fb)
            (lift fc) () subj

        method gmap fa fb fc subj =
          gmap_triple_fix.call Igmap_triple.Triple (lift fa) (lift fb)
            (lift fc) () subj

        method eval = eval_triple_0 eval_triple_fix

        method stateful = stateful_triple_0 stateful_triple_fix

        method compare = compare_triple_0 compare_triple_fix

        method eq = eq_triple_0 eq_triple_fix

        method foldl = foldl_triple_0 foldl_triple_fix

        method foldr = foldr_triple_0 foldr_triple_fix
  end
 }

let tuple3 = triple

class virtual ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] tuple3_t = object
  inherit     ['ia,'a,'sa, 'ib,'b,'sb, 'ic,'c,'sc, 'inh, 'e, 'syn] triple_t
end
let gcata_tuple3 = gcata_triple

class ['a, 'b, 'c, 'self] show_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] show_triple_t _fix fa fb fc fself
end
let show_tuple3_fix = show_triple_fix
class ['a, 'b, 'c, 'self] fmt_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[fmt] _fix fa fb fc fself
end
let fmt_tuple3_fix = fmt_triple_fix
class ['a, 'b, 'c, 'self] html_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[html] _fix fa fb fc fself
end
let html_tuple3_fix = html_triple_fix
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'self] gmap_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'self] @triple[gmap] _fix fa fb fc fself
end
let gmap_tuple3_fix = gmap_triple_fix
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] eval_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] @triple[eval] _fix fa fb fc fself
end
let eval_tuple3_fix = eval_triple_fix
class ['a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] stateful_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'a2, 'b, 'b2, 'c, 'c2, 'env, 'self] @triple[stateful] _fix fa fb fc fself
end
let stateful_tuple3_fix = stateful_triple_fix
class ['a, 'b, 'c, 'self] compare_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] compare_triple_t _fix fa fb fc fself
end
let compare_tuple3_fix = compare_triple_fix
class ['a, 'b, 'c, 'self] eq_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'self] @triple[eq] _fix fa fb fc fself
end
let eq_tuple3_fix = eq_triple_fix
class ['a, 'b, 'c, 'syn, 'self] foldl_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] @triple[foldl] _fix fa fb fc fself
end
let foldl_tuple3_fix = foldl_triple_fix
class ['a, 'b, 'c, 'syn, 'self] foldr_tuple3_t _fix fa fb fc fself = object
  inherit [ 'a, 'b, 'c, 'syn, 'self] @triple[foldr] _fix fa fb fc fself
end
let foldr_tuple3_fix = foldr_triple_fix

(*******************************************************************************)
(* Tuples of size 4 *)
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
