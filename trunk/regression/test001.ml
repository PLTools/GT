open GT

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
    inherit [string, unit, string] t_t
    method c_R  () _     = "R"
    method c_W  () _     = "W"
    method c_L  () _ x   = "L " ^ x
    method c_S  () _ x   = "S " ^ x
    method c_B  () _ _ x = "B " ^ x
    method c_E  () _     = "E"
    method c_C  () _ x   = "C "  ^ (string_of_int x)
    method c_J  () _ x   = "J "  ^ (x.fx ())
    method c_JT () _ x   = "JT " ^ (x.fx ())
    method c_JF () _ x   = "JF " ^ (x.fx ())
  end

class ['a] map_t =
  object (this)
    inherit ['a, unit, 'a t] t_t
    method c_R  _ _     = R
    method c_W  _ _     = W
    method c_L  _ _ x   = L x
    method c_S  _ _ x   = S x
    method c_B  _ _ f x = B (f, x)
    method c_E  _ _     = E
    method c_C  _ _ x   = C x
    method c_J  _ _ x   = J  (x.fx ())
    method c_JT _ _ x   = JT (x.fx ())
    method c_JF _ _ x   = JF (x.fx ())
  end

class resolve =
  object (this)
    inherit [int] map_t
  end

class string_t =
  object (this)
    inherit [string] map_t
  end

let resolve p = 
  let symbols = ref [] in
  let p = Array.mapi (fun i (s, c) -> if s != "" then symbols := (s, i) :: !symbols; c) p in
  Array.map (fun i -> t.transform_t (*gcata*) (fun _ i -> List.assoc i !symbols) (new resolve) () i) p

let toString f i  = t.transform_t f (new toString) () i

type env  = int list * (string -> int) * int list * int list * int

class interpret =
  object (this)
    inherit [int, env, env option] t_t    
    method c_R  (      s, m, x::i, o, p) _     = Some (x::s, m, i, o, p+1)
    method c_W  (   x::s, m,    i, o, p) _     = Some (s, m, i, x::o, p+1)
    method c_L  (      s, m,    i, o, p) _ x   = Some ((m x)::s, m, i, o, p+1)
    method c_S  (   y::s, m,    i, o, p) _ x   = Some (s, (fun z -> if z = x then y else m z), i, o, p+1)
    method c_B  (y::z::s, m,    i, o, p) _ f _ = Some ((f z y)::s, m, i, o, p+1)
    method c_E   _ _                           = None
    method c_C  (      s, m,    i, o, p) _ n   = Some (n::s, m, i, o, p+1)
    method c_J  ((      s, m,    i, o, p) as env) _ n   = Some (s, m, i, o, (*~:n*) n.GT.fx env)
    method c_JT ((   x::s, m,    i, o, p) as env) _ n   = Some (s, m, i, o, if x != 0 then (*~:n*) n.GT.fx env else p+1)
    method c_JF ((   x::s, m,    i, o, p) as env) _ n   = Some (s, m, i, o, if x  = 0 then (*~:n*) n.GT.fx env  else p+1)   
  end

class debug callback =
  object (this)
    inherit interpret as super
    method c_R  c i     = callback c; super#c_R  c i
    method c_W  c i     = callback c; super#c_W  c i
    method c_L  c i x   = callback c; super#c_L  c i x
    method c_S  c i x   = callback c; super#c_S  c i x
    method c_B  c i x y = callback c; super#c_B  c i x y
    method c_E  c i     = callback c; super#c_E  c i 
    method c_C  c i x   = callback c; super#c_C  c i x
    method c_J  c i x   = callback c; super#c_J  c i x
    method c_JT c i x   = callback c; super#c_JT c i x
    method c_JF c i x   = callback c; super#c_JF c i x
  end

let interpret ii p i =
  let rec inner (_, _, _, o, i) as conf  =
    match t.transform_t (*gcata*) (fun _ i -> i) ii conf p.(i) with
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
  let log = ref [] in
  let print p =
    List.iter (fun i -> Printf.printf "%s @ %d\n" (toString (fun _ x -> string_of_int x) p.(i)) i) (List.rev !log);
    log := []
  in
  let dd = new debug (fun (_, _, _, _, p) -> log := p :: !log) in
  let main name xx p i = 
    Printf.printf "%s:\n" name;
    List.iter (fun x -> Printf.printf "%d\n" x) (interpret xx p i) 
  in  
  main "sum"  ii sum [2; 3];
  main "sumN" ii sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];

  main "sum with debug"  dd sum [2; 3];
  print sum;

  main "sumN with debug" dd sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  print sumN;
  
  main "sum"  ii (resolve sumS) [2; 3];
  main "sumN" ii (resolve sumNS) [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
