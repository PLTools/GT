open GT

let show_string () s = Printf.sprintf "\"%s\"" s

type lam =
  | Var of string
  | App   of lam*lam
  | Lam   of string* lam
[@@deriving gt ~show ]

type context = string -> string
type mtype = context -> lam -> lam

class virtual ['self] reducer = object (self: 'self)
  inherit [ 'inh, 'syn, 'extra] class_lam
  constraint 'inh = lam
  constraint 'syn = lam

  method virtual arg: mtype
  method virtual subst_arg : mtype
  method head : mtype = fun c x -> x.fx c
  method c_Var ctx s = ctx s
  method c_App c left right =
    match this#head c left with
    | Lam (x,l') -> s.f c (subst c x (this#subst arg c m) l')
    | l' -> let l'' = s.f c l'  in App(l'' ,this#arg c m)
end

