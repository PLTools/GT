@type a = [`A of GT.int | `B of GT.string] deriving show, eq, compare
@type b = [`C of GT.int | `D of GT.string] deriving show, eq, compare
@type c = [a | b] deriving show, eq, compare

 
