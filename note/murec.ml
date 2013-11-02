type m = M of int | N of n
and  n = K of m | L of string

let xm = N (K (N (L "1")))
let xn = K xm

class virtual ['i, 's] m_t =
  object (self) 
    method virtual m_M   : 'i -> (('i -> m -> 's) * m) -> (('i -> int -> 's) * int) -> 's
    method virtual m_N   : 'i -> (('i -> m -> 's) * m) -> (('i -> n -> 's) * n) -> 's
    method virtual t_n   : 'i -> n -> 's
    method virtual t_int : 'i -> int -> 's
    method virtual t_m   : 'i -> m -> 's
  end

let rec transform_m : 'i 's . ('i, 's) #m_t -> 'i -> m -> 's = fun t acc x ->
  let self = transform_m t in
  match x with
  | M y -> t#m_M acc (self, x) (t#t_int, y)
  | N y -> t#m_N acc (self, x) (t#t_n, y)

class virtual ['i, 's] n_t =
  object (self)
    method virtual m_K      : 'i -> (('i -> n -> 's) * n) -> (('i -> m -> 's) * m) -> 's  
    method virtual m_L      : 'i -> (('i -> n -> 's) * n) -> (('i -> string -> 's) * string) -> 's
    method virtual t_m      : 'i -> m -> 's
    method virtual t_string : 'i -> string -> 's
    method virtual t_n      : 'i -> n -> 's
  end

let rec transform_n : 'i 's . ('i, 's) #n_t -> 'i -> n -> 's = fun t acc x ->
  let self = transform_n t in
  match x with
  | K y -> t#m_K acc (self, x) (t#t_m, y)
  | L y -> t#m_L acc (self, x) (t#t_string, y)

class show_m' env =
  object (self)
    inherit [unit, string] m_t
    method m_M _ (_, _) (fi, i) = Printf.sprintf "M (%s)" (fi () i)
    method m_N _ (_, _) (fn, n) = Printf.sprintf "N (%s)" (fn () n)
    method t_int _ i = string_of_int i
    method t_n = env#t_n
    method t_m _ m = transform_m self () m
  end

class show_n' env =
  object (self)
    inherit [unit, string] n_t
    method m_K _ (_, _) (fn, n) = Printf.sprintf "K (%s)" (fn () n)
    method m_L _ (_, _) (fs, s) = Printf.sprintf "L (%s)" (fs () s)
    method t_m = env#t_m
    method t_string acc s = s
    method t_n _ n = transform_n self () n
  end

class show_m =
  object (this)
    method m_M   = (new show_m' this)#m_M
    method m_N   = (new show_m' this)#m_N
    method t_int = (new show_m' this)#t_int
    method t_m   = transform_m this
    method t_n   = transform_n (new show_n' this)      
  end

class show_n =
  object (this)
    method m_K      = (new show_n' this)#m_K
    method m_L      = (new show_n' this)#m_L
    method t_string = (new show_n' this)#t_string
    method t_m      = transform_m (new show_m' this)
    method t_n      = transform_n this      
  end

class show_m'' = 
  object (this)
    inherit show_m
    method m_N _ _ (fn, n) = Printf.sprintf "NNN (%s)" (fn () n)
  end

class show_n'' = 
  object (this)
    inherit show_n
    method m_L _ _ (fn, n) = Printf.sprintf "LLL (%s)" (fn () n)
  end

let _ =   
  Printf.printf "%s\n" (transform_m (new show_m) () xm);
  Printf.printf "%s\n" (transform_m (new show_m'') () xm);
  Printf.printf "%s\n" (transform_n (new show_n) () xn);
  Printf.printf "%s\n" (transform_n (new show_n'') () xn)


