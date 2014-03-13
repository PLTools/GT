@type ('a, 'b) a = A of 'a | B of 'b deriving show, map
@type ('a, 'b) t = X of ('a, 'b) a * ('a, 'b) t GT.list | 
                   Y of ('b, 'a) a * ('a, 'b) t GT.list deriving show, map
