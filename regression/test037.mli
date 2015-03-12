@type 'a t1 = [`A | `B of 'a] with show, map
@type 'a t2 = [`C | `D of 'a] with show, map
@type 'a t  = ['a t1 | 'a t2] with show, map
