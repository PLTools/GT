open GT

type tree = Node of int * tree list
[@@deriving gt]

class ['a, 'b] tree_map_t fself f =
  object
    inherit ['a, 'b, _] class_tree
    method c_Node _ n l = Node (f n, List.map (fun t -> fself () t) l)
  end

class ['a, 'b] tree_fold_t fself f =
  object
    inherit ['a, 'b, _] class_tree
    method c_Node acc n l = List.fold_left (fun acc t -> fself acc t) (f acc n) l
  end

let num_of_nodes t =
  GT.fix0 (fun self -> transform(tree) (new tree_fold_t self (fun n _ -> n+1)) )
    0 t

let increment t =
  GT.fix0 (fun self -> transform(tree) (new tree_map_t self (fun x -> x + 1)) )
    () t

let toString t =
  Buffer.contents @@
  GT.fix0 (fun self ->
    GT.transform(tree)
      (new tree_fold_t self
        (fun buf n ->
           Buffer.add_string buf (string_of_int n);
           buf
        )
      )
  )
    (Buffer.create 1024)
    t


let _ = 
  let t = Node (1, [Node (2, [Node (4, [])]); Node (2, [])]) in
  Printf.printf "Tree: %s\n" (toString t);
  Printf.printf "Number of nodes: %d\n" (num_of_nodes t);
  Printf.printf "Incremented: %s\n" (toString (increment t))

