(* @type 'a tree = Node of 'a * 'a tree GT.list with (\* show, *\) foldl, foldr *)

(* let _ =
 *   let x  = Node (1, [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, [])])]) in
 *   let n  = GT.transform(tree) (new @tree[foldl] (+)) 0 x in
 *
 *   let fa s x = if s = "" then string_of_int x else s ^ ", " ^ string_of_int x in
 *   let sl = GT.transform(tree) (new @tree[foldl] @@ fa) "" x in
 *   let sr = GT.transform(tree) (new @tree[foldr] @@ fa) "" x in
 *   Printf.printf "%s\n" (GT.transform(tree) (new @tree[show] (GT.lift string_of_int)) () x);
 *   Printf.printf "%d\n" n;
 *   Printf.printf "%s\n" sl;
 *   Printf.printf "%s\n" sr *)

type 'a tree =
    Node of 'a * 'a tree GT.list
class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] tree_t =
  object
    method virtual c_Node : 'inh -> 'a tree -> 'a -> 'a tree GT.list -> 'syn
  end
let gcata_tree tr inh subj =
  match subj with
    Node (_x__001_, _x__002_) -> tr#c_Node inh subj _x__001_ _x__002_
module type IndexResult_tree =
  sig
    type 'a result
    type 'dummy0 i = Tree : ('a result -> 'a tree result) i
  end
module Index_tree (S : sig type 'a result end) =
  struct
    type 'a result = 'a S.result
    type 'dummy0 i = Tree : ('a result -> 'a tree result) i
  end

module type IndexResult2_tree =
sig
    type ('a, 'b) result
    type 'dummy0 i = Tree : (('a, 'a2) result -> ('a tree, 'a2 tree) result) i
  end
module Index2_tree (S : sig type ('a, 'b) result end) =
  struct
    type ('a, 'b) result = ('a, 'b) S.result
    type 'dummy0 i = Tree : (('a, 'a2) result -> ('a tree, 'a2 tree) result) i
  end

(* for fold *)
module type IndexResult3_tree =
  sig
    type ('a, 'b) result
    type 'dummy0 i = Tree : (('a, 'a2) result -> ('a tree, 'a2) result) i
  end
module Index3_tree (S : sig type ('a, 'b) result end) =
  struct
    type ('a, 'b) result = ('a, 'b) S.result
    type 'dummy0 i = Tree : (('a, 'a2) result -> ('a tree, 'a2) result) i
  end

module Ifoldr_tree =
  Index3_tree (struct type ('a, 'b) result = 'b -> 'a -> 'b end)
module Fix_foldr_tree = GT.FixV (Ifoldr_tree)
class ['a, 'syn, 'extra_tree] foldr_tree_t _
    fa fself_tree =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'extra_tree, 'syn] tree_t
    method c_Node inh___003_ _ _x__004_ _x__005_ =
      fa
        ((fun inh subj -> GT.foldr GT.list fself_tree inh subj) inh___003_
           _x__005_)
        _x__004_
  end
let foldr_tree_0 call fa inh0 subj =
  GT.transform_gc gcata_tree ((new foldr_tree_t) call fa) inh0 subj
let foldr_tree_fix =
  Fix_foldr_tree.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ifoldr_tree.i)  ->
           (match sym with Ifoldr_tree.Tree -> foldr_tree_0 f : a)})

let tree =
  {GT.gcata = gcata_tree;
   GT.plugins =
     object (_)
       method foldr = foldr_tree_0 foldr_tree_fix
     end}
