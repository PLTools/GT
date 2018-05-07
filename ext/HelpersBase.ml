let id x = x
let raise_errorf ?loc fmt = Printf.ksprintf failwith fmt
let not_implemented ?loc fmt =
  Printf.ksprintf (raise_errorf ~loc "%s are not yet implemented") fmt

module List = struct
  include Base.List
  let split3 xs =
    List.fold_right (fun (a,b,c) (ac,bc,cc) -> (a::ac,b::bc,c::cc))
      xs ([],[],[])
  let filter_map ~f xs =
    List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []
  let last_exn xs = List.hd @@ List.rev xs

  let pp ~f xs =
    Printf.sprintf "[ %s ]" (String.concat "; " @@ List.map f xs)

  let fold_left0 f = function
  | [] -> failwith "wrong argument of fold_left0"
  | h::tl -> fold_left ~f ~init:h tl

  let concat_map ~f xs = List.concat @@ List.map f xs
  let empty = function [] -> true | _ -> false
end

module Format = struct
  include Caml.Format
  let easy_string f x =
    let (_:string) = flush_str_formatter () in
    f str_formatter x;
    flush_str_formatter ()

end
