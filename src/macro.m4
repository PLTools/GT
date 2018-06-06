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
class ['extra] gmap_$1_t _fself =
  object
    inherit [unit, 'extra, $1] @$1
    method t_$1 _ x = x
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
class ['env, 'extra] eval_$1_t _fself =
  object
    inherit ['env, 'extra, '$1] @$1
    method t_$1 inh x = x
  end
class ['env, 'extra] stateful_$1_t _fself =
  object
    inherit ['env, 'extra, 'env * $1] @$1
    method t_$1 inh x = (inh,x)
  end

let gcata_$1 tr inh x = tr#t_$1 inh x

let $1 : (('inh, _, 'syn) # $1_t -> 'inh -> $1 -> 'syn,
          < show    : $1 -> string;
            html    : $1 -> HTML.viewer;
            compare : $1 -> $1 -> comparison;
            eq      : $1 -> $1 -> bool;
            gmap    : $1 -> $1;
            eval    : 'env -> $1 -> $1;
            stateful: 'env -> $1 -> 'env * $1;
            foldl   : 'a -> $1 -> 'a;
            foldr   : 'a -> $1 -> 'a >) t =
  {gcata = gcata_$1;
   plugins =
      object
        method show    = gcata_$1 (new @$1[show]    (fun _ -> assert false)) ()
        method html    = gcata_$1 (new @$1[html]    (fun _ -> assert false)) ()
        method compare = gcata_$1 (new @$1[compare] (fun _ -> assert false))
        method eq      = gcata_$1 (new @$1[eq]      (fun _ -> assert false))
        method gmap    = gcata_$1 (new @$1[gmap]    (fun _ -> assert false)) ()
        method eval    = gcata_$1 (new @$1[eval]    (fun _ -> assert false))
        method stateful= gcata_$1 (new @$1[stateful](fun _ -> assert false))
        method foldl   = gcata_$1 (new @$1[foldl]   (fun _ -> assert false))
        method foldr   = gcata_$1 (new @$1[foldr]   (fun _ -> assert false))
      end
  }
]])
