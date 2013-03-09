generic tree = Node of [int] * [tree list]

class virtual ['a, 'b] tree_t =
  object(self)
    method virtual m_Node : 'a -> tree -> int -> tree list -> 'b
  end

class ['a, 'b] tree_map_t f =
  object(self)
    inherit ['a, 'b] tree_t
    method m_Node _ _ n l = Node (f n, List.map (fun (Node (n, l)) as t -> self#m_Node () t n l ) l)
  end

class ['a, 'b] tree_fold_t f =
  object(self)
    inherit ['a, 'b] tree_t
    method m_Node acc _ n l = List.fold_left (fun acc (Node (n, l)) as t -> self#m_Node acc t n l) (f acc n) l
  end

let num_of_nodes t = 
  tree.Generic.gcata (*Generic.apply*) (new tree_fold_t (fun n _ -> n+1))
    0 t

let increment t =
  tree.Generic.gcata (*Generic.apply*) (new tree_map_t (fun x -> x + 1))
    () t

let toString t =
  Buffer.contents (
    tree.Generic.gcata (*Generic.apply*) 
      (new tree_fold_t (fun buf n -> 
                          Buffer.add_string buf (string_of_int n);
                          buf
                       )
      ) 
      (Buffer.create 1024) 
      t
  )

let _ = 
  let t = Node (1, [Node (2, [Node (4, [])]); Node (2, [])]) in
  Printf.printf "Tree: %s\n" (toString t);
  Printf.printf "Number of nodes: %d\n" (num_of_nodes t);
  Printf.printf "Incremented: %s\n" (toString (increment t))

