type ('a, 'b) t = int * (string * ('a * 'b))
 (* with show, gmap, eq, compare, foldr, foldl *)


class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'extra] t_t :
  object
    inherit
      [int,int,int,(string * ('a * 'b)),(string * ('ia * 'ib)),(string * ('sa
                                                                 * 'sb)),
      'inh,'syn,'extra] GT.tuple2_t
  end
val gcata_t :
  ('a,_,'sa,'b,_,'sb,'inh,'syn,_)#t_t -> 'inh -> ('a, 'b) t -> 'syn
class ['a,'b,'syn,'extra] foldl_t_t :
  ('syn -> ('a, 'b) t -> 'syn) ->
    ('syn -> 'a -> 'syn) ->
      ('syn -> 'b -> 'syn) ->
        object
          inherit [int,(string * ('a * 'b)),'syn,'extra] GT.foldl_tuple2_t
        end
val foldl_t :
  ('syn -> 'a -> 'syn) -> ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) t -> 'syn
class ['a,'b,'syn,'extra] foldr_t_t :
  ('syn -> ('a, 'b) t -> 'syn) ->
    ('syn -> 'a -> 'syn) ->
      ('syn -> 'b -> 'syn) ->
        object
          inherit [int,(string * ('a * 'b)),'syn,'extra] GT.foldr_tuple2_t
        end
val foldr_t :
  ('syn -> 'a -> 'syn) -> ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) t -> 'syn
class ['a,'b,'extra] compare_t_t :
  (('a, 'b) t -> ('a, 'b) t -> GT.comparison) ->
    ('a -> 'a -> GT.comparison) ->
      ('b -> 'b -> GT.comparison) ->
        object inherit [int,(string * ('a * 'b)),'extra] GT.compare_tuple2_t
        end
val compare_t :
  ('a -> 'a -> GT.comparison) ->
    ('b -> 'b -> GT.comparison) -> ('a, 'b) t -> ('a, 'b) t -> GT.comparison
class ['a,'b,'extra] eq_t_t :
  (('a, 'b) t -> ('a, 'b) t -> bool) ->
    ('a -> 'a -> bool) ->
      ('b -> 'b -> bool) ->
        object inherit [int,(string * ('a * 'b)),'extra] GT.eq_tuple2_t end
val eq_t :
  ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
class ['a,'a_2,'b,'b_2,'extra] gmap_t_t :
  (('a, 'b) t -> ('a_2, 'b_2) t) ->
    ('a -> 'a_2) ->
      ('b -> 'b_2) ->
        object
          inherit
            [int,int,(string * ('a * 'b)),(string * ('a_2 * 'b_2)),'extra]
            GT.gmap_tuple2_t
        end
val gmap_t : ('a -> 'a_2) -> ('b -> 'b_2) -> ('a, 'b) t -> ('a_2, 'b_2) t
class ['a,'b,'extra] show_t_t :
  (('a, 'b) t -> string) ->
    ('a -> string) ->
      ('b -> string) ->
        object inherit [int,(string * ('a * 'b)),'extra] GT.show_tuple2_t end
val show_t : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
val t :
  (('a,_,'sa,'b,_,'sb,'inh,'syn,_)#t_t -> 'inh -> ('a, 'b) t -> 'syn,
    <
      foldl: ('syn -> 'a -> 'syn) ->
               ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) t -> 'syn  ;foldr:
                                                                    ('syn ->
                                                                    'a ->
                                                                    'syn) ->
                                                                    ('syn ->
                                                                    'b ->
                                                                    'syn) ->
                                                                    'syn ->
                                                                    ('a,
                                                                    'b) t ->
                                                                    'syn  ;
      compare: ('a -> 'a -> GT.comparison) ->
                 ('b -> 'b -> GT.comparison) ->
                   ('a, 'b) t -> ('a, 'b) t -> GT.comparison  ;eq: ('a ->
                                                                    'a ->
                                                                    bool) ->
                                                                    ('b ->
                                                                    'b ->
                                                                    bool) ->
                                                                    ('a,
                                                                    'b) t ->
                                                                    ('a,
                                                                    'b) t ->
                                                                    bool  ;
      gmap: ('a -> 'a_2) -> ('b -> 'b_2) -> ('a, 'b) t -> ('a_2, 'b_2) t  ;
      show: ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string   > )
    GT.t
