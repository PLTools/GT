changequote([[,]])

define(GENERIFY, [[
type proto$1 = $1
type $1 = proto$1

class virtual ['inh, 'self, 'syn] $1_t =
  object
    method virtual t_$1 : 'inh -> $1 -> 'syn
  end
let gcata_$1 tr inh x = tr#t_$1 inh x

module type IndexResult_$1 = sig
  type 'a result
  type 'dummy0 i = The_$1 : $1 result i
end
module Index_$1 (S : sig type 'a result end) =
struct
  type 'a result = 'a S.result
  type 'dummy0 i = The_$1 : $1 result i
end
module type IndexResult2_$1 = sig
  type ('a, 'b) result
  type 'dummy0 i = The_$1 : ($1, $1) result i
end
module Index2_$1 (S : sig type ('a, 'b) result end) = struct
  type ('a, 'b) result = ('a, 'b) S.result
  type 'dummy0 i = The_$1 : ($1, $1) result i
end
module type IndexResult_fold_$1 = sig
  type ('a, 'syn) result
  type 'dummy0 i = The_$1 : ($1, 'syn) result i
end
module Index_fold_$1 (S : sig type ('a, 'syn) result end) = struct
  type ('a, 'syn) result = ('a, 'syn) S.result
  type 'dummy0 i = The_$1 : ($1, 'syn) result i
end
module type IndexResult_stateful_$1 = sig
  type ('env, 'a, 'b) result
  type 'dummy0 i = The_$1 : ('env, $1, $1) result i
end
module Index_stateful_$1 (S : sig type ('env, 'a, 'b) result end) =
struct
  type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
  type 'dummy0 i = The_$1 : ('env, $1, $1) result i
end

module Ishow_$1 = Index_$1 (struct type 'a result = unit -> 'a -> string end)
module Fix_show_$1 = FixV (Ishow_$1)
class ['extra] show_$1_t _fix _fself = object
  inherit [unit, 'extra, string] @$1
  method t_$1 inh x = string_of_$1 x
end
let show_$1_0 call = transform_gc gcata_$1 (new show_$1_t call)
let show_$1_fix = Fix_show_$1.fixv
  (fun f -> {call =
     fun (type a) (sym : a Ishow_$1.i) ->
       (match sym with Ishow_$1.The_$1 -> show_$1_0 f : a)})

module Ihtml_$1 = Index_$1 (struct type 'a result = unit -> 'a -> HTML.er end)
module Fix_html_$1 = FixV (Ihtml_$1)
class ['extra] html_$1_t _ _fself =
  object
    inherit [unit, 'extra, HTML.viewer] @$1
    method t_$1 inh x = HTML.string (string_of_$1 x)
  end
let html_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new html_$1_t) call) inh0 subj
let html_$1_fix =
  Fix_html_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ihtml_$1.i) ->
           (match sym with Ihtml_$1.The_$1 -> html_$1_0 f : a)})

module Ifmt_$1 = Index_$1 (struct
  type 'a result = Format.formatter -> 'a -> unit
  end)
module Fix_fmt_$1 = FixV (Ifmt_$1)
class ['extra] fmt_$1_t _ _fself =
  object
    inherit [Format.formatter, 'extra, unit] @$1
    method t_$1 fmt x = Format.pp_print_$1 fmt x
  end
let fmt_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new fmt_$1_t) call) inh0 subj
let fmt_$1_fix = Fix_fmt_$1.fixv (fun f ->
  {call =
    fun (type a) (sym : a Ifmt_$1.i) ->
      (match sym with Ifmt_$1.The_$1 -> fmt_$1_0 f : a)})

module Igmap_$1 = Index2_$1 (struct type ('a, 'b) result = unit -> 'a -> 'b end)
module Fix_gmap_$1 = FixV (Igmap_$1)
class ['extra] gmap_$1_t _ _fself =
  object
    inherit [unit, 'extra, $1] @$1
    method t_$1 _ x = x
  end
let gmap_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new gmap_$1_t) call) inh0 subj
let gmap_$1_fix = Fix_gmap_$1.fixv (fun f ->
  {call =
    fun (type a) (sym : a Igmap_$1.i) ->
      (match sym with Igmap_$1.The_$1 -> gmap_$1_0 f : a)})

module Ifoldl_$1 =
  Index_fold_$1 (struct type ('a, 'b) result = 'b -> 'a -> 'b end)
module Fix_foldl_$1 = FixV (Ifoldl_$1)
class ['syn, 'extra] foldl_$1_t _ _fself =
  object
    inherit ['syn, 'extra, 'syn] @$1
    method t_$1 s _ = s
  end
let foldl_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new foldl_$1_t) call) inh0 subj
let foldl_$1_fix =
  Fix_foldl_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ifoldl_$1.i) ->
           (match sym with Ifoldl_$1.The_$1 -> foldl_$1_0 f : a)})

module Ifoldr_$1 =
  Index_fold_$1 (struct type ('a, 'b) result = 'b -> 'a -> 'b end)
module Fix_foldr_$1 = FixV (Ifoldr_$1)
class ['syn, 'extra] foldr_$1_t _fix _fself =
  object
    inherit ['syn, 'extra, 'syn] @$1
    method t_$1 s _ = s
  end
let foldr_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new foldr_$1_t) call) inh0 subj
let foldr_$1_fix =
  Fix_foldr_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ifoldr_$1.i) ->
           (match sym with Ifoldr_$1.The_$1 -> foldr_$1_0 f : a)})

module Ieq_$1 = Index_$1 (struct type 'a result = 'a -> 'a -> bool end)
module Fix_eq_$1 = FixV (Ieq_$1)
class ['extra] eq_$1_t _fix _fself =
  object
    inherit [$1, 'extra, 'bool] @$1
    method t_$1 inh x = x = inh
  end
let eq_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new eq_$1_t) call) inh0 subj
let eq_$1_fix =
  Fix_eq_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ieq_$1.i) ->
           (match sym with Ieq_$1.The_$1 -> eq_$1_0 f : a)})

module Icompare_$1 =
  Index_$1 (struct type 'a result = 'a -> 'a -> comparison end)
module Fix_compare_$1 = FixV (Icompare_$1)
class ['extra] compare_$1_t _fix _fself =
  object
    inherit [$1, 'extra, 'comparison] @$1
    method t_$1 inh x = compare_primitive inh x
  end
let compare_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new compare_$1_t) call) inh0 subj
let compare_$1_fix =
  Fix_compare_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Icompare_$1.i) ->
           (match sym with Icompare_$1.The_$1 -> compare_$1_0 f : a)})

module Ieval_$1 =
  Index2_$1 (struct type ('a, 'b) result = unit -> 'a -> 'b end)
module Fix_eval_$1 = FixV (Ieval_$1)
class ['env, 'extra] eval_$1_t _fix _fself =
  object
    inherit ['env, 'extra, '$1] @$1
    method t_$1 inh x = x
  end
let eval_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new eval_$1_t) call) inh0 subj
let eval_$1_fix =
  Fix_eval_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ieval_$1.i) ->
           (match sym with Ieval_$1.The_$1 -> eval_$1_0 f : a)})

module Istateful_$1 =
  Index_stateful_$1
    (struct type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b end)
module Fix_stateful_$1 = FixV (Istateful_$1)
class ['env, 'extra] stateful_$1_t _fix _fself =
  object
    inherit ['env, 'extra, 'env * $1] @$1
    method t_$1 inh x = (inh,x)
  end
let stateful_$1_0 call inh0 subj =
  transform_gc gcata_$1 ((new stateful_$1_t) call) inh0 subj
let stateful_$1_fix =
  Fix_stateful_$1.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Istateful_$1.i) ->
           (match sym with Istateful_$1.The_$1 -> stateful_$1_0 f : a)})


let $1 : (('inh, _, 'syn) # $1_t -> 'inh -> $1 -> 'syn,
          < show    : $1 -> string;
            html    : $1 -> HTML.viewer;
            fmt     : Format.formatter -> $1 -> unit;
            compare : $1 -> $1 -> comparison;
            eq      : $1 -> $1 -> bool;
            gmap    : $1 -> $1;
            eval    : 'env -> $1 -> $1;
            stateful: 'env -> $1 -> 'env * $1;
            foldl   : 'a -> $1 -> 'a;
            foldr   : 'a -> $1 -> 'a >) t =
  {gcata = gcata_$1;
   plugins = object
       method show subj = show_$1_fix.call Ishow_$1.The_$1 () subj
       method html subj = html_$1_fix.call Ihtml_$1.The_$1 () subj
       method fmt = fmt_$1_0 fmt_$1_fix
       method gmap subj = gmap_$1_fix.call Igmap_$1.The_$1 () subj
       method foldr = foldr_$1_0 foldr_$1_fix
       method foldl = foldl_$1_0 foldl_$1_fix
       method eq = eq_$1_0 eq_$1_fix
       method compare = compare_$1_0 compare_$1_fix
       method eval = eval_$1_0 eval_$1_fix
       method stateful = stateful_$1_0 stateful_$1_fix
      end
  }
]])
