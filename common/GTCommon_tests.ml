open GTCommon.HelpersBase

let%test _ =
  let loc = Location.none in
  [ "a" ] = (vars_from_core_type [%type: 'a list] |> SS.elements)
;;

let%test _ =
  let loc = Location.none in
  [] = (vars_from_core_type [%type: int list] |> SS.elements)
;;
