open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
(* [@@deriving gt ~options:{show}] *)

class virtual ['il,'l,'sl,'inh,'self,'syn] a_t =
  object
    method virtual  c_A : 'inh -> b -> 'syn
    method virtual  c_C : 'inh -> 'syn
    method virtual  c_E : 'inh -> 'l a -> 'syn
    method virtual  c_D : 'inh -> 'l -> 'syn
  end
class virtual ['inh,'self,'syn] b_t =
  object
    method virtual  c_I : 'inh -> GT.int a -> 'syn
    method virtual  c_J : 'inh -> 'syn
    method virtual  c_K : 'inh -> b -> 'syn
  end
let gcata_a tr inh subj =
  match subj with
  | A _x__001_ -> tr#c_A inh _x__001_
  | C -> tr#c_C inh
  | E _x__002_ -> tr#c_E inh _x__002_
  | D _x__003_ -> tr#c_D inh _x__003_
let gcata_b tr inh subj =
  match subj with
  | I _x__004_ -> tr#c_I inh _x__004_
  | J -> tr#c_J inh
  | K _x__005_ -> tr#c_K inh _x__005_
class ['l,'self_a] show_a_t_stub oshow_b  fself  fl =
  object
    inherit  [unit,'l,string,unit,'self_a,string] a_t
    method c_A inh___006_ _x__007_ =
      Printf.sprintf "A(%s)"
        ((fun subj -> gcata_b (oshow_b ()) () subj) _x__007_)
    method c_C inh___008_ = "C"
    method c_E inh___009_ _x__010_ = Printf.sprintf "E(%s)" (fself _x__010_)
    method c_D inh___011_ _x__012_ = Printf.sprintf "D(%s)" (fl _x__012_)
  end
class ['self_b] show_b_t_stub oshow_a  fself =
  object
    inherit  [unit,'self_b,string] b_t
    method c_I inh___013_ _x__014_ =
      Printf.sprintf "I(%s)"
        ((fun subj ->
            gcata_a (oshow_a ()) ()
              (fun subj -> (GT.int.GT.plugins)#show subj) subj) _x__014_)
    method c_J inh___015_ = "J"
    method c_K inh___016_ _x__017_ = Printf.sprintf "K(%s)" (fself _x__017_)
  end
type nonrec show_t_a_1 = {
  show_a_trf: 'l . ('l -> string) -> 'l a -> string }
type nonrec show_t_a_2 =
  {
  show_oa_func:
    'self_a 'l . unit -> ('l -> string) -> ('l, 'self_a) show_a_t_stub }
type nonrec show_t_a_3 =
  {
  show_a_func:
    'self_a 'l 'self_b .
      (unit -> 'self_b show_b_t_stub) ->
        ('l a -> string) -> ('l -> string) -> ('l, 'self_a) show_a_t_stub
    }
type nonrec show_t_b_1 = {
  show_b_trf: b -> string }
type nonrec show_t_b_2 =
  {
  show_ob_func: 'self_b . unit -> 'self_b show_b_t_stub }
type nonrec show_t_b_3 =
  {
  show_b_func:
    'self_b 'self_a .
      (unit -> (GT.int -> string) -> (GT.int, 'self_a) show_a_t_stub) ->
        (b -> string) -> 'self_b show_b_t_stub
    }
let show_fix_a_b (a0, b0) =
  let rec show_a =
    {
      show_a_trf =
        (fun fl -> fun subj -> gcata_a (oa.show_oa_func () fl) () subj)
    }
  and oa =
    {
      show_oa_func =
        (fun () ->
           fun fl -> a0.show_a_func ob.show_ob_func (show_a.show_a_trf fl) fl)
    }
  and show_b =
    { show_b_trf = (fun subj -> gcata_b (ob.show_ob_func ()) () subj) }
  and ob =
    {
      show_ob_func =
        (fun () -> b0.show_b_func oa.show_oa_func show_b.show_b_trf)
    } in
  (show_a, oa, show_b, ob)
let (fix_result_a, o_a, fix_result_b, o_b) =
  show_fix_a_b
    ({ show_a_func = (new show_a_t_stub) },
      { show_b_func = (new show_b_t_stub) })
let show_a fl subj = fix_result_a.show_a_trf fl subj
let show_b subj = fix_result_b.show_b_trf subj
class ['l,'self_a] show_a_t fself  fl =
  object inherit  ((['l,'self_a] show_a_t_stub) oshow_b fself fl) end
class ['self_b] show_b_t fself =
  object inherit  ((['self_b] show_b_t_stub) oshow_a fself) end


let _ =
  printf "%s\n" @@ show_a (GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);
  ()
