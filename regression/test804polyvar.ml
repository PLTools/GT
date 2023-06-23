module L : sig
  type 'a lst =
    [ `Nil
    | `Cons of 'a * 'a lst
    ]
  [@@deriving gt ~options:{ gmap }]
end = struct
  type 'a lst =
    [ `Nil
    | `Cons of 'a * 'a lst
    ]
  [@@deriving gt ~options:{ gmap }]

  let __
    :  (('inh -> 'c lst -> 'syn) -> ('e, 'c, 'f, 'inh, [> 'c lst ], 'syn) #lst_t) -> 'inh
    -> 'c lst -> 'syn
    =
   fun eta -> GT.transform_gc gcata_lst eta
 ;;
end

type 'a maybe =
  | Just of 'a
  | Nothing
[@@deriving gt ~options:{ show; fmt }]

type 'a pv = [ `A of 'a ] [@@deriving gt ~options:{ show; fmt }]

let () =
  let sh x = GT.show pv Fun.id x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")
;;

include (
  struct
    type 'a wtf = [ `C of 'a | 'a pv ] maybe [@@deriving gt ~options:{ show; fmt }]
  end :
    sig
      type 'a wtf = [ `C of 'a | 'a pv ] maybe [@@deriving gt ~options:{ show; fmt }]
    end)

let () =
  let sh x = GT.show wtf Fun.id x in
  Printf.printf "%s\t%s\n%!" (sh @@ Just (`A "a")) (sh @@ Just (`C "ccc"))
;;
