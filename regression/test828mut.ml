class virtual ['ia, 'a, 'sa, 'inh, 'syn] list_t =
  object
    method virtual c_Nil : 'inh -> 'syn
    method virtual c_Cons : 'inh -> 'a -> 'a list -> 'syn
  end

let gcata_list tr inh s =
  match s with
  | [] -> tr#c_Nil inh
  | x :: xs -> tr#c_Cons inh x xs
;;

class ['a, 'sa, 'syn] gmap_list_t fa fself =
  object
    constraint 'syn = 'sa list
    inherit [unit, 'a, 'sa, unit, 'syn] list_t
    method c_Nil _ = []
    method c_Cons _ x xs = fa () x :: fself () xs
  end

class virtual ['ia, 'a, 'sa, 'inh, 'syn] option_t =
  object
    method virtual c_None : 'inh -> 'syn
    method virtual c_Some : 'inh -> 'a -> 'syn
  end

let gcata_option tr inh subj =
  match subj with
  | None -> tr#c_None inh
  | Some x -> tr#c_Some inh x
;;

class ['a, 'sa, 'syn] gmap_option_t fa _fself =
  object
    constraint 'syn = 'sa option
    inherit [unit, 'a, 'sa, unit, 'syn] option_t
    method c_None () = None
    method c_Some () x = Some (fa () x)
  end

include (
  struct
    type 'a aaa = 'a bbb GT.option
    and 'a bbb = 'a bbb GT.list [@@deriving gt ~options:{ gmap }]

    let __ (type a b) : (a -> b) -> a bbb -> b bbb = fun eta -> GT.gmap bbb eta
  end :
    sig
      type 'a aaa = 'a bbb GT.option
      and 'a bbb = 'a bbb GT.list [@@deriving gt ~options:{ gmap }]
    end)
