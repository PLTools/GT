module List = struct
  include ListLabels
  let split3 xs =
    List.fold_right (fun (a,b,c) (ac,bc,cc) -> (a::ac,b::bc,c::cc))
      xs ([],[],[])
  let filter_map ~f xs =
    List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []
  let last_exn xs = List.hd @@ List.rev xs
end

module Exp = struct
  open Ast_helper
  include Exp

  let make_pair a b = tuple [a;b]
  let make_list =
    List.fold_right ~f:(fun e acc -> construct (lid "::") (Some (make_pair e acc)) )
                    ~init:(Exp.construct (lid "[]") None)

  let fun_list ~args e =
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Exp.fun_ Nolabel None arg acc)
end

module Cl = struct
  open Ast_helper
  include Cl

  let fun_list args e =
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ Asttypes.Nolabel None arg acc)
end

module Typ = struct
  open Ast_helper
  include Typ
  let ground s = constr (lid s) []
end
open Parsetree

let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
   | _ -> failwith "bad argument of map_type_param_names")
