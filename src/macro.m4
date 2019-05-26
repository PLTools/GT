changequote([[,]])

define(GENERIFY, [[
type proto$1 = $1
type $1 = proto$1

class virtual ['inh, 'self, 'syn] $1_t =
  object
    method virtual t_$1 : 'inh -> $1 -> 'syn
  end

class ['extra] html_$1_t _fself =
  object
    inherit [unit, 'extra, HTML.viewer] @$1
    method t_$1 inh x = HTML.string (string_of_$1 x)
  end
class ['extra] show_$1_t _fself =
  object
    inherit [unit, 'extra, string] @$1
    method t_$1 inh x = string_of_$1 x
  end
class ['extra] fmt_$1_t _fself =
  object
    inherit [Format.formatter, 'extra, unit] @$1
    method t_$1 fmt x = Format.pp_print_$1 fmt x
  end
class ['syn, 'extra] foldl_$1_t _fself =
  object
    inherit ['syn, 'extra, 'syn] @$1
    method t_$1 s _ = s
  end
class ['syn, 'extra] foldr_$1_t _fself =
  object
    inherit ['syn, 'extra, 'syn] @$1
    method t_$1 s _ = s
  end
class ['extra] eq_$1_t _fself =
  object
    inherit [$1, 'extra, 'bool] @$1
    method t_$1 inh x = x = inh
  end
class ['extra] compare_$1_t _fself =
  object
    inherit [$1, 'extra, 'comparison] @$1
    method t_$1 inh x = compare_primitive inh x
  end

class ['extra, 'syn] gmap_$1_t _fself =
  object
    constraint 'syn = $1
    inherit [unit, 'extra, 'syn'] @$1
    method t_$1 _ x = x
  end
class [ 'extra, 'syn, 'env] eval_$1_t _fself =
  object
    constraint 'syn = $1
    inherit ['env, 'extra, '$1] @$1
    method t_$1 inh x = x
  end
class [ 'extra, 'syn, 'env] stateful_$1_t _fself =
  object
    constraint 'syn = $1
    inherit ['env, 'extra, 'env * $1] @$1
    method t_$1 inh x = (inh,x)
  end

let gcata_$1 tr inh x = tr#t_$1 inh x

let $1 : (('inh, $1, 'syn) # $1_t -> 'inh -> $1 -> 'syn,
          < show    : $1 -> string;
            html    : $1 -> HTML.viewer;
            fmt     : Format.formatter -> $1 -> unit;
            compare : $1 -> $1 -> comparison;
            eq      : $1 -> $1 -> bool;
            gmap    : $1 -> $1;
            eval    : 'env -> $1 -> $1;
            stateful: 'env -> $1 -> 'env * $1;
            foldl   : 'a -> $1 -> 'a;
            foldr   : 'a -> $1 -> 'a >,
            (('inh -> $1 -> 'syn) -> ('inh, $1, 'syn) $1_t) -> 'inh -> $1 -> 'syn) t =
  {gcata = gcata_$1;
   fix = (fun c -> transform_gc gcata_$1 c);
   plugins =
      object
        method show    = transform_gc gcata_$1 (new @$1[show]    ) ()
        method gmap    = transform_gc gcata_$1 (new @$1[gmap]    ) ()
        method html    = transform_gc gcata_$1 (new @$1[html]    ) ()
        method fmt     = transform_gc gcata_$1 (new @$1[fmt]     )
        method compare = transform_gc gcata_$1 (new @$1[compare] )
        method eq      = transform_gc gcata_$1 (new @$1[eq]      )
        method eval    = transform_gc gcata_$1 (new @$1[eval]    )
        method stateful= transform_gc gcata_$1 (new @$1[stateful])
        method foldl   = transform_gc gcata_$1 (new @$1[foldl]   )
        method foldr   = transform_gc gcata_$1 (new @$1[foldr]   )
      end
  }
]])
