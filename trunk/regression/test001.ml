generic 'l t = 
    R 
  | W 
  | L  of [string] 
  | S  of [string] 
  | B  of [int -> int -> int] * [string]
  | E
  | C  of [int]
  | J  of 'l
  | JT of 'l
  | JF of 'l

class toString =
  object (this)
    inherit [int, string, unit, string] t_t
    method m_R  () _     = "R"
    method m_W  () _     = "W"
    method m_L  () _ x   = "L " ^ x
    method m_S  () _ x   = "S " ^ x
    method m_B  () _ _ x = "B " ^ x
    method m_E  () _     = "E"
    method m_C  () _ x   = "C "  ^ (string_of_int x)
    method m_J  () _ x   = "J "  ^ (x.Generic.f ())
    method m_JT () _ x   = "JT " ^ (x.Generic.f ())
    method m_JF () _ x   = "JF " ^ (x.Generic.f ())
  end

class resolve =
  object (this)
    inherit [string, int, unit, int t] t_t
    method m_R  _ _     = R
    method m_W  _ _     = W
    method m_L  _ _ x   = L x
    method m_S  _ _ x   = S x
    method m_B  _ _ f x = B (f, x)
    method m_E  _ _     = E
    method m_C  _ _ x   = C x
    method m_J  _ _ x   = J  (x.Generic.f ())
    method m_JT _ _ x   = JT (x.Generic.f ())
    method m_JF _ _ x   = JF (x.Generic.f ())
  end

let resolve p = 
  let symbols = ref [] in
  let p = Array.mapi (fun i (s, c) -> if s != "" then symbols := (s, i) :: !symbols; c) p in
  Array.map (fun i -> t.Generic.gcata (new resolve) (fun _ i -> List.assoc i !symbols) () i) p

let toString i  = t.Generic.gcata (new toString) (fun _ i -> string_of_int i) () i

type env  = int list * (string -> int) * int list * int list * int

class interpret =
  object (this)
    inherit [int, int, env, env option] t_t    
    method m_R  (      s, m, x::i, o, p) _     = Some (x::s, m, i, o, p+1)
    method m_W  (   x::s, m,    i, o, p) _     = Some (s, m, i, x::o, p+1)
    method m_L  (      s, m,    i, o, p) _ x   = Some ((m x)::s, m, i, o, p+1)
    method m_S  (   y::s, m,    i, o, p) _ x   = Some (s, (fun z -> if z = x then y else m z), i, o, p+1)
    method m_B  (y::z::s, m,    i, o, p) _ f _ = Some ((f z y)::s, m, i, o, p+1)
    method m_E   _ _                           = None
    method m_C  (      s, m,    i, o, p) _ n   = Some (n::s, m, i, o, p+1)
    method m_J  (      s, m,    i, o, p) _ n   = Some (s, m, i, o, n.Generic.x)
    method m_JT (   x::s, m,    i, o, p) _ n   = Some (s, m, i, o, if x != 0 then n.Generic.x else p+1)
    method m_JF (   x::s, m,    i, o, p) _ n   = Some (s, m, i, o, if x  = 0 then n.Generic.x else p+1)   
  end

class debug callback =
  object (this)
    inherit interpret as super
    method m_R  c i     = callback i c; super#m_R  c i
    method m_W  c i     = callback i c; super#m_W  c i
    method m_L  c i x   = callback i c; super#m_L  c i x
    method m_S  c i x   = callback i c; super#m_S  c i x
    method m_B  c i x y = callback i c; super#m_B  c i x y
    method m_E  c i     = callback i c; super#m_E  c i 
    method m_C  c i x   = callback i c; super#m_C  c i x
    method m_J  c i x   = callback i c; super#m_J  c i x
    method m_JT c i x   = callback i c; super#m_JT c i x
    method m_JF c i x   = callback i c; super#m_JF c i x
  end

let interpret ii p i =
  let rec inner (_, _, _, o, i) as conf  =
    match t.Generic.gcata ii (fun _ i -> i) conf p.(i) with
    | None      -> List.rev o
    | Some conf -> inner conf
  in
  inner ([], (fun x -> invalid_arg (Printf.sprintf "Variable %s undefined" x)), i, [], 0)

let sum  = [|R; R; B ((+), "+"); W; E|]
let sumN = [|
  R;
  S  "n";
  C   0 ;
  S  "s";
  L  "n"; (* cont *)
  JF  15;
  L  "n";
  C   1 ;
  B  ((-), "-");
  S  "n";
  R;
  L  "s";
  B  ((+), "+");
  S  "s";
  J   4 ;
  L  "s"; (* end *)
  W;
  E
|]

let sumS = [|"", R; "", R; "", B ((+), "+"); "", W; "", E|]
let sumNS = [|
  ""    , R;
  ""    , S  "n";
  ""    , C   0 ;
  ""    , S  "s";
  "cont", L  "n"; 
  ""    , JF  "end";
  ""    , L  "n";
  ""    , C   1 ;
  ""    , B  ((-), "-");
  ""    , S  "n";
  ""    , R;
  ""    , L  "s";
  ""    , B  ((+), "+");
  ""    , S  "s";
  ""    , J  "cont";
  "end" , L  "s"; 
  ""    , W;
  ""    , E
|]

let _ = 
  let ii = new interpret in
  let dd = new debug (fun i (_, _, _, _, p) -> Printf.printf "%s @ %d\n" (toString i.Generic.x) p) in
  let main name xx p i = 
    Printf.printf "%s:\n" name;
    List.iter (fun x -> Printf.printf "%d\n" x) (interpret xx p i) 
  in  
  main "sum"  ii sum [2; 3];
  main "sumN" ii sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  main "sum with debug"  dd sum [2; 3];
  main "sumN with debug" dd sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  main "sum"  ii (resolve sumS) [2; 3];
  main "sumN" ii (resolve sumNS) [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];

