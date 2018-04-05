open GT

let show_string () s = Printf.sprintf "\"%s\"" s

type lam =
  | Var of string
  | App   of lam*lam
  | Lam   of string* lam
[@@deriving gt ~show ]

let subst ctx name new_ old =
  let rec helper = function
    | Var s when s=name -> new_
    | App (l,r) -> App (helper l, helper r)
    | Lam (s, body) as term when s = name -> term
    | Lam (s, body) -> Lam (s, helper body)
    | term -> term
  in
  helper old

type context = string -> string
type mtype = context -> lam -> lam

class virtual reducer fself = object (self: 'self)
  inherit [ 'inh, 'syn, 'extra] class_lam
  constraint 'inh = context
  constraint 'syn = lam

  (* Reduce argument when head function was _not_ reduced to abstraction*)
  method virtual arg: mtype
  (* Reduce argument when head function was reduced to abstraction*)
  method virtual subst_arg : mtype
  method head : mtype = fun ctx lam -> fself ctx lam
  method c_Var ctx name = fself ctx (Var name)
  method c_App ctx left right =
    (* assert false *)
    match self#head ctx left with
    | Lam (name, l') -> fself ctx (subst ctx name (self#subst_arg ctx right) l')
    | term ->
      let l'' = fself ctx term  in
      App(l'', self#arg ctx right)
end

class virtual strict fself = object
  inherit reducer fself
  method subst_arg ctx term = fself ctx term
end
class virtual non_strict fself = object(self: 'self)
  inherit reducer fself
  (* In non-strict we do nothing with arguments at the moment *)
  method subst_arg _ctx term = term
end
class virtual reduce_arguments fself = object
  inherit reducer fself
  method arg ctx l = fself ctx l
end
class virtual dont_reduce_under_abstractions fself = object
  inherit reducer fself
  method  c_Lam ctx v body = fself ctx (Lam (v,body))
end
class call_by_value fself  = object
  inherit dont_reduce_under_abstractions fself
  inherit reduce_arguments fself
  inherit strict fself
end

(* class applicative fself = object
 *   inherit call_by_value fself
 *   inherit reduce_under_abstractions fself
 * end *)

let rec eval_cbv env lam : lam =
  gcata_lam (new call_by_value eval_cbv) env lam
