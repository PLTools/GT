type a = [ `A of GT.int | `B of GT.string ]
class virtual ['inh, 'self, 'syn] a_t =
  object
    method virtual c_A : 'inh -> ([> a ] as 'self) -> GT.int -> 'syn
    method virtual c_B : 'inh -> ([> a ] as 'self) -> GT.string -> 'syn
  end
let gcata_a tr inh subj =
  match subj with
    `A ___001_ -> tr#c_A inh subj ___001_
  | `B ___002_ -> tr#c_B inh subj ___002_
class ['self_a] compare_a_t fself =
  object
    inherit [[> a ] as 'self_a, 'self_a, GT.comparison] a_t
    method c_A inh___003_ _ _x__004_ =
      match inh___003_ with
        `A _x__005_ ->
          GT.chain_compare GT.EQ
            (fun () ->
               (fun inh subj -> GT.int.GT.plugins#compare inh subj) _x__005_
                 _x__004_)
      | other -> GT.compare_vari other (`A (Obj.magic ()))
    method c_B inh___006_ _ _x__007_ =
      match inh___006_ with
        `B _x__008_ ->
          GT.chain_compare GT.EQ
            (fun () ->
               (fun inh subj -> GT.string.GT.plugins#compare inh subj)
                 _x__008_ _x__007_)
      | other -> GT.compare_vari other (`B (Obj.magic ()))
  end
let rec compare_a the_init subj =
  GT.fix0 (fun self -> gcata_a ((new compare_a_t) self)) the_init subj
let a =
  {GT.gcata = gcata_a; GT.plugins = object (_) method compare = compare_a end}
type b = [ `C of GT.int | `D of GT.string ]
class virtual ['inh, 'self, 'syn] b_t =
  object
    method virtual c_C : 'inh -> ([> b ] as 'self) -> GT.int -> 'syn
    method virtual c_D : 'inh -> ([> b ] as 'self) -> GT.string -> 'syn
  end
let gcata_b tr inh subj =
  match subj with
    `C ___009_ -> tr#c_C inh subj ___009_
  | `D ___010_ -> tr#c_D inh subj ___010_
class ['self_b] compare_b_t fself =
  object
    inherit [[> b ] as 'self_b, 'self_b, GT.comparison] b_t
    method c_C inh___011_ _ _x__012_ =
      match inh___011_ with
        `C _x__013_ ->
          GT.chain_compare GT.EQ
            (fun () ->
               (fun inh subj -> GT.int.GT.plugins#compare inh subj) _x__013_
                 _x__012_)
      | other -> GT.compare_vari other (`C (Obj.magic ()))
    method c_D inh___014_ _ _x__015_ =
      match inh___014_ with
        `D _x__016_ ->
          GT.chain_compare GT.EQ
            (fun () ->
               (fun inh subj -> GT.string.GT.plugins#compare inh subj)
                 _x__016_ _x__015_)
      | other -> GT.compare_vari other (`D (Obj.magic ()))
  end
let rec compare_b the_init subj =
  GT.fix0 (fun self -> gcata_b ((new compare_b_t) self)) the_init subj
let b =
  {GT.gcata = gcata_b; GT.plugins = object (_) method compare = compare_b end}
type c = [ a | b ]
class virtual ['inh, 'self, 'syn] c_t =
  object inherit ['inh, 'self, 'syn] a_t inherit ['inh, 'self, 'syn] b_t end
let gcata_c tr inh subj =
  match subj with
    #a as subj -> gcata_a tr inh subj
  | #b as subj -> gcata_b tr inh subj
class ['self_c] compare_c_t fself =
  object
    inherit [[> c ] as 'self_c, 'self_c, GT.comparison] c_t
    inherit ['self_c] compare_a_t fself
    inherit ['self_c] compare_b_t fself
  end

(* let (_: int) = (new compare_c_t (fun _ -> assert false))#c_C *)

(* let (_: (c -> c -> _) -> int) = new compare_c_t *)
let rec compare_c the_init subj =
  GT.transform1_gc gcata_c (new compare_c_t) the_init subj
  (* GT.fix0 (fun self -> gcata_c ((new compare_c_t) self)) the_init subj *)
let c =
  {GT.gcata = gcata_c; GT.plugins = object (_) method compare = compare_c end}


(* @type a = [`A of GT.int | `B of GT.string] with (\* show, eq, *\) compare
 * @type b = [`C of GT.int | `D of GT.string] with (\* show, eq, *\) compare
 * @type c = [a | b] with (\* show, eq, *\) compare
 *
 * let _ =
 *   let x = `A 3 in
 *   let y = `D "2" in
 *   Printf.printf "%s\n" @@ GT.transform (a) (new @a[show]) x;
 *   Printf.printf "%s\n" @@ GT.transform (b) (new @b[show]) y;
 *   Printf.printf "%s\n" @@ GT.transform (c) (new @c[show]) x;
 *   Printf.printf "%s\n" @@ GT.transform (c) (new @c[show]) y;
 *   Printf.printf "%b\n" @@ GT.transform1(a) (new @a[eq]) x x;
 *   Printf.printf "%b\n" @@ GT.transform1(b) (new @b[eq]) y y;
 *   Printf.printf "%b\n" @@ GT.transform1(c) (new @c[eq]) x x;
 *   Printf.printf "%b\n" @@ GT.transform1(c) (new @c[eq]) y y;
 *   Printf.printf "%b\n" @@ GT.transform1(c) (new @c[eq]) x y; *)
