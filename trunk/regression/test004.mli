@type a = [`A of b | `C of GT.int] deriving show
and   b = [`B of a | `D of GT.string] deriving show
