@type 'a a = [`A of 'a | `B of GT.string] deriving show, eq, compare
@type 'a b = [`C of 'a | `D of GT.string] deriving show, eq, compare
@type 'a c = ['a a | 'a b] deriving show, eq, compare
