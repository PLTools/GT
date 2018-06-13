
let int =
  { GT.gcata = GT.gcata_int;
    GT.plugins = object
      method show = GT.int.GT.plugins#show
      method html = GT.int.GT.plugins#html
      method gmap = GT.int.GT.plugins#gmap
      method compare = GT.int.GT.plugins#compare
      method eq      = GT.int.GT.plugins#eq
      method foldl = GT.int.GT.plugins#foldr
      method foldr = GT.int.GT.plugins#foldl
      method stateful = GT.int.GT.plugins#stateful
      method eval = GT.int.GT.plugins#eval
      method show_typed x = GT.int.GT.plugins#show x
    end
  }
let string =
  { GT.gcata = GT.gcata_string;
    GT.plugins = object
      method show = GT.string.GT.plugins#show
      method html = GT.string.GT.plugins#html
      method gmap = GT.string.GT.plugins#gmap
      method compare = GT.string.GT.plugins#compare
      method eq      = GT.string.GT.plugins#eq
      method foldl = GT.string.GT.plugins#foldr
      method foldr = GT.string.GT.plugins#foldl
      method stateful = GT.string.GT.plugins#stateful
      method eval     = GT.string.GT.plugins#eval
      method show_typed x = GT.string.GT.plugins#show x
    end
  }

class ['a, 'b, 'self] show_typed_tuple2_t _ _typa fa _typb fb =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] GT.pair_t
    method c_Pair () x y = "(" ^ fa x ^ ", " ^ fb y ^ ")"
  end

class ['a, 'self] show_typed_free_t fself _typa fa =
  object
    inherit ['a, 'self] GT.show_free_t fself fa
  end

class ['a, 'self] show_typed_list_t fself _typa fa = object
  inherit ['a, 'self] GT.show_list_t fself fa
end
class ['a, 'self] show_typed_option_t fself _typa fa = object
  inherit ['a, 'self] GT.show_option_t fself fa
end

(* module Lazy = struct
 *   include GT.Lazy
 *   class ['a, 'self] show_typed_lazy_t fself _typa fa = object
 *     inherit ['a, 'self] GT.Lazy.show_t_t fself fa
 *   end
 * end *)

class ['a, 'b, 'c, 'self] show_typed_tuple3_t fself _typa fa _typb fb _typc fc =
  object
    inherit ['a, 'b, 'c, 'self] GT.show_tuple3_t fself fa fb fc
  end

let tuple2 =
  { GT.gcata = GT.gcata_pair;
    GT.plugins = object
      method show = GT.tuple2.GT.plugins#show
      method html = GT.tuple2.GT.plugins#html
      method gmap = GT.tuple2.GT.plugins#gmap
      method compare = GT.tuple2.GT.plugins#compare
      method eq      = GT.tuple2.GT.plugins#eq
      method foldl = GT.tuple2.GT.plugins#foldr
      method foldr = GT.tuple2.GT.plugins#foldl
      method stateful = GT.tuple2.GT.plugins#stateful
      method eval = GT.tuple2.GT.plugins#eval
      method show_typed _typa fa _typb fb x =
        GT.transform(GT.pair)
          (new show_typed_tuple2_t (fun _ _ -> assert false)
            _typa fa _typb fb
          )
          () x
    end
  }
