type 'a a = [ `A of 'a | `B of GT.string ]
and ('self, 'a) a_open =
  'self constraint 'self = [> `A of 'a | `B of GT.string ]
class type virtual ['a, 'ia, 'sa, 'inh, 'syn] a_tt =
  object
    method c_A :
      'inh -> ('inh, 'a a, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method c_B :
      'inh -> ('inh, 'a a, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        GT.string -> 'syn
    method t_a : ('ia -> 'a -> 'sa) -> 'inh -> 'a a -> 'syn
  end
val a :
  (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #a_tt -> 'inh -> 'a a ->
    'syn, < show : ('a_ -> string) -> 'a_ a -> string;
  eq : ('ax -> 'a_ -> bool) -> ('self, 'a) a_open -> 'a_ a -> bool;
  compare :
    ('a -> 'a_ -> GT.comparison) -> ('self, 'a) a_open -> 'a_ a ->
      GT.comparison >)
    GT.t
class virtual ['a, 'ia, 'sa, 'inh, 'syn] a_t :
  object
    method virtual c_A :
      'inh -> ('inh, 'a a, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method virtual c_B :
      'inh -> ('inh, 'a a, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        GT.string -> 'syn
    method t_a : ('ia -> 'a -> 'sa) -> 'inh -> 'a a -> 'syn
  end
class type ['a] show_a_env_tt = object  end
class type ['self, 'a] eq_a_env_tt = object  end
class type ['self, 'a] compare_a_env_tt = object  end
class ['a] show_proto_a :
  'a show_a_env_tt ref ->
    object inherit ['a, unit, string, unit, string] a_tt end
class ['self, 'a] eq_proto_a :
  ('self, 'a) eq_a_env_tt ref ->
    object inherit ['a, 'a, bool, ('self, 'a) a_open, bool] a_tt end
class ['self, 'a] compare_proto_a :
  ('self, 'a) compare_a_env_tt ref ->
    object
      inherit ['a, 'a, GT.comparison, ('self, 'a) a_open, GT.comparison] a_tt
    end
class ['a] show_a_t :
  object
    inherit ['a, unit, string, unit, string] a_tt
    inherit ['a] show_a_env_tt
  end
class ['self, 'a] eq_a_t :
  object
    inherit ['a, 'a, bool, ('self, 'a) a_open, bool] a_tt
    inherit ['self, 'a] eq_a_env_tt
  end
class ['self, 'a] compare_a_t :
  object
    inherit ['a, 'a, GT.comparison, ('self, 'a) a_open, GT.comparison] a_tt
    inherit ['self, 'a] compare_a_env_tt
  end
type b = [ `C of GT.int | `D of GT.string ]
and 'self b_open =
  'self constraint 'self = [> `C of GT.int | `D of GT.string ]
class type virtual ['inh, 'syn] b_tt =
  object
    method c_C : 'inh -> ('inh, b, 'syn, < >) GT.a -> GT.int -> 'syn
    method c_D : 'inh -> ('inh, b, 'syn, < >) GT.a -> GT.string -> 'syn
    method t_b : 'inh -> b -> 'syn
  end
val b :
  (('inh, 'syn) #b_tt -> 'inh -> b -> 'syn, < show : b -> string; eq : 'self b_open -> b -> bool;
  compare : 'self b_open -> b -> GT.comparison >)
    GT.t
class virtual ['inh, 'syn] b_t :
  object
    method virtual c_C : 'inh -> ('inh, b, 'syn, < >) GT.a -> GT.int -> 'syn
    method virtual c_D :
      'inh -> ('inh, b, 'syn, < >) GT.a -> GT.string -> 'syn
    method t_b : 'inh -> b -> 'syn
  end
class type show_b_env_tt = object  end
class type ['self] eq_b_env_tt = object  end
class type ['self] compare_b_env_tt = object  end
class show_proto_b :
  show_b_env_tt ref -> object inherit [unit, string] b_tt end
class ['self] eq_proto_b :
  'self eq_b_env_tt ref -> object inherit ['self b_open, bool] b_tt end
class ['self] compare_proto_b :
  'self compare_b_env_tt ref ->
    object inherit ['self b_open, GT.comparison] b_tt end
class show_b_t : object inherit [unit, string] b_tt inherit show_b_env_tt end
class ['self] eq_b_t :
  object
    inherit ['self b_open, bool] b_tt
    inherit ['self] eq_b_env_tt
  end
class ['self] compare_b_t :
  object
    inherit ['self b_open, GT.comparison] b_tt
    inherit ['self] compare_b_env_tt
  end
type 'a c = [ 'a a | b ]
and ('self, 'a) c_open = 'self constraint 'self = [> 'a a | b ]
class type virtual ['a, 'ia, 'sa, 'inh, 'syn] c_tt =
  object
    inherit ['a, 'ia, 'sa, 'inh, 'syn] a_tt
    inherit ['inh, 'syn] b_tt
    method t_c : ('ia -> 'a -> 'sa) -> 'inh -> 'a c -> 'syn
  end
val c :
  (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #c_tt -> 'inh -> 'a c ->
    'syn, < show : ('a -> string) -> 'a c -> string;
  eq : ('a -> 'a -> bool) -> ('self, 'a) c_open -> 'a c -> bool;
  compare :
    ('a -> 'a -> GT.comparison) -> ('self, 'a) c_open -> 'a c ->
      GT.comparison >)
    GT.t
class virtual ['a, 'ia, 'sa, 'inh, 'syn] c_t :
  object
    inherit ['a, 'ia, 'sa, 'inh, 'syn] a_t
    inherit ['inh, 'syn] b_t
    method t_c : ('ia -> 'a -> 'sa) -> 'inh -> 'a c -> 'syn
  end
class type ['a] show_c_env_tt =
  object
    inherit show_b_env_tt
    inherit ['a] show_a_env_tt
  end
class type ['self, 'a] eq_c_env_tt =
  object
    inherit ['self] eq_b_env_tt
    inherit ['self, 'a] eq_a_env_tt
  end
class type ['self, 'a] compare_c_env_tt =
  object
    inherit ['self] compare_b_env_tt
    inherit ['self, 'a] compare_a_env_tt
  end
class ['a] show_proto_c :
  'a show_c_env_tt ref ->
    object inherit ['a, unit, string, unit, string] c_tt end
class ['self, 'a] eq_proto_c :
  ('self, 'a) eq_c_env_tt ref ->
    object inherit ['a, 'a, bool, ('self, 'a) c_open, bool] c_tt end
class ['self, 'a] compare_proto_c :
  ('self, 'a) compare_c_env_tt ref ->
    object
      inherit ['a, 'a, GT.comparison, ('self, 'a) c_open, GT.comparison] c_tt
    end
class ['a] show_c_t :
  object
    inherit ['a, unit, string, unit, string] c_tt
    inherit ['a] show_c_env_tt
  end
class ['self, 'a] eq_c_t :
  object
    inherit ['a, 'a, bool, ('self, 'a) c_open, bool] c_tt
    inherit ['self, 'a] eq_c_env_tt
  end
class ['self, 'a] compare_c_t :
  object
    inherit ['a, 'a, GT.comparison, ('self, 'a) c_open, GT.comparison] c_tt
    inherit ['self, 'a] compare_c_env_tt
  end
