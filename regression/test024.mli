@type 'a a = [`A of 'a | `B of GT.string] deriving show, eq, compare
@type 'a b = [`C of 'a | `D of GT.string] deriving show, eq, compare
@type ('a, 'b) c = ['a a | 'b b] deriving show, eq, compare
