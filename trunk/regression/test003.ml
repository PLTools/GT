@type ('a, 'b) t = A of ['a] | B of ['b] | C of [('b, 'a) t] deriving show
