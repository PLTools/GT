@type a = A of b | C of int deriving show
and   b = B of a | D of string deriving show
