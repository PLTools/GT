(* @type a = A of b | C of GT.int GT.list with show
 * and   b = B of a | D of GT.string      with show *)

type a =
    A of b
  | C of GT.int GT.list
and b =
    B of a
  | D of GT.string

class virtual ['inh, 'self, 'syn] a_t =
  object
    method virtual c_A : 'inh -> a -> b -> 'syn
    method virtual c_C : 'inh -> a -> GT.int GT.list -> 'syn
  end
class virtual ['inh, 'self, 'syn] b_t =
  object
    method virtual c_B : 'inh -> b -> a -> 'syn
    method virtual c_D : 'inh -> b -> GT.string -> 'syn
  end
let gcata_a tr inh subj =
  match subj with
    A _x__001_ -> tr#c_A inh subj _x__001_
  | C _x__002_ -> tr#c_C inh subj _x__002_
let gcata_b tr inh subj =
  match subj with
    B _x__003_ -> tr#c_B inh subj _x__003_
  | D _x__004_ -> tr#c_D inh subj _x__004_
type show_t_a_1 = { show_a_trf : a -> string }
type show_t_b_1 = { show_b_trf : b -> string }
type show_a_prereq = { show_a : show_t_a_1; show_b : show_t_b_1 }
class ['self_a] show_a_t_stub mut_trfs_here fself =
  object
    inherit [unit, 'self_a, string] a_t
    method c_A inh___005_ _ _x__006_ =
      Printf.sprintf "A (%s)" (mut_trfs_here.show_b.show_b_trf () _x__006_)
    method c_C inh___007_ _ _x__008_ =
      Printf.sprintf "C (%s)"
        ((fun () subj ->
            GT.list.GT.plugins#show
              ((fun () subj -> GT.int.GT.plugins#show subj) ()) subj)
           () _x__008_)
  end
class ['self_b] show_b_t_stub mut_trfs_here fself =
  object
    inherit [unit, 'self_b, string] b_t
    method c_B inh___009_ _ _x__010_ =
      Printf.sprintf "B (%s)" (mut_trfs_here.show_a.show_a_trf () _x__010_)
    method c_D inh___011_ _ _x__012_ =
      Printf.sprintf "D (%s)"
        ((fun () subj -> GT.string.GT.plugins#show subj) () _x__012_)
  end
type show_t_a_3 =
  { show_a_func :
      'self_a .
        show_a_prereq -> (unit -> a -> string) -> 'self_a show_a_t_stub }
type show_t_b_3 =
  { show_b_func :
      'self_b .
        show_a_prereq -> (unit -> b -> string) -> 'self_b show_b_t_stub }
let show_fix_a ?(a0 = {show_a_func = new show_a_t_stub})
    ?(b0 = {show_b_func = new show_b_t_stub}) () =
  let rec show_a =
    {show_a_trf =
      fun inh subj ->
        gcata_a
          (a0.show_a_func {show_a = show_a; show_b = show_b}
             show_a.show_a_trf)
          inh subj}
  and show_b =
    {show_b_trf =
      fun inh subj ->
        gcata_b
          (b0.show_b_func {show_a = show_a; show_b = show_b}
             show_b.show_b_trf)
          inh subj}
  in
  {show_a = show_a; show_b = show_b}
let ({show_a = fix_result_a; show_b = fix_result_b} as all_trfs_together) =
  show_fix_a ()
let show_a subj = fix_result_a.show_a_trf () subj
let show_b subj = fix_result_b.show_b_trf () subj
class ['self_a] show_a_t fself =
  object inherit ['self_a] show_a_t_stub all_trfs_together fself end
class ['self_b] show_b_t fself =
  object inherit ['self_b] show_b_t_stub all_trfs_together fself end
let a = {GT.gcata = gcata_a; GT.plugins = object (_) method show = show_a end}
let b = {GT.gcata = gcata_b; GT.plugins = object (_) method show = show_b end}





class show_a2stub prereq fself =
  object
    inherit [_] show_a_t_stub prereq fself as super
    method c_C () a ys = "new " ^ super#c_C () a ys
  end

let show_a_new =
  let { show_a } = show_fix_a ~a0:{ show_a_func = new show_a2stub } ()
  in
  show_a.show_a_trf ()

let a = { a with plugins = object
                   method show = show_a_new
                 end}
let _ =
  let x = A (B (C [1; 2; 3; 4])) in
  let y = B (A (D "3")) in
  Printf.printf "%s\n" (GT.show(a) x);
  Printf.printf "%s\n" (GT.show(b) y);
  Printf.printf "%s\n" (GT.show(a) x);
