@type a = [`A of b | `C of GT.int GT.list] deriving show
and   b = [`B of a | `D of GT.string] deriving show

