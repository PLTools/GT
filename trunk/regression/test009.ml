 generic ident = [ `Var of string ]
 
class ['v] ident_eval = object
  inherit [string -> 'v, 'v] ident_t
  method m_Var s id = s id
end
      
generic 'a arith = [`Add of 'a * 'a | `Sub of 'a * 'a]

class ['a, 'inh] arith_eval = object
  inherit ['a, int, 'inh, int] arith_t
  method m_Add self inh x y = self inh x + self inh y
  method m_Sub self inh x y = self inh x - self inh y
end

generic expr = [ ident | expr arith ]

 