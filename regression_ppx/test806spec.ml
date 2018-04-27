open GT

type 'a maybe = Just of 'a | Nothing [@@deriving gt ~show ]

module P = struct
  type t = (int -> string) maybe [@@deriving gt
    ~show:{ _1 = (fun _ -> "<fun>") }
]
end

let () =
  Printf.printf "%s\n%!" @@ P.show_t (Just (fun x -> "?"))
