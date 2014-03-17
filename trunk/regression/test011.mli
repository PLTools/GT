@type var = [`Var of GT.string] deriving show
@type 'a lambda = [var | `Abs of GT.string * 'a | `App of 'a * 'a] deriving show
@type 'a var_expr = [var | `Num of GT.int | `Add of 'a * 'a | `Mult of 'a * 'a] deriving show
@type 'a expr = ['a lambda | 'a var_expr] deriving show
