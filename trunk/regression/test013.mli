@type a = [`A of GT.int | `B of b] deriving show
and   b = [`C of GT.string | `D of a] deriving show

@type c = [a | b] deriving show
