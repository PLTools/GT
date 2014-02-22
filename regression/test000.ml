open GT

@type tree = Node of int * tree list

class ['a, 'b] tree_map_t f =
  object
    inherit ['a, 'b] @tree
    method c_Node _ x n l = Node (f n, List.map (fun t -> x.f () t) l)
  end

class ['a, 'b] tree_fold_t f =
  object
    inherit ['a, 'b] @tree
    method c_Node acc x n l = List.fold_left (fun acc t -> x.f acc t) (f acc n) l
  end

let num_of_nodes t = 
  transform(tree) (new tree_fold_t (fun n _ -> n+1))
    0 t

let increment t =
  transform(tree) (new tree_map_t (fun x -> x + 1))
    () t

let toString t =
  Buffer.contents (
    transform(tree)
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

