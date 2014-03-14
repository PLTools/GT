@type a = A of b | C of GT.int GT.list deriving show
and   b = B of c | D of GT.string deriving show
and   c = E of a deriving show
