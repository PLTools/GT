type t =  A of GT.int [@@deriving gt ~options:{show}]

let () = Format.printf "Should be an ADT:   `%s`\n%!" (GT.show t (A 5))

(* Let's override implementation to use fancy printing *)
let t =
  {
    GT.gcata = gcata_t;
    GT.fix = (fun eta -> GT.transform_gc gcata_t eta);
    GT.plugins = (object method show (A n) = string_of_int n end)
  }

let () = Format.printf "Should be a number: `%s`\n%!" (GT.show t (A 5))

(* By default t2 uses inheritance from class_t_t and output is default one *)
type t2 = t [@@deriving gt ~options:{show}]
let () = Format.printf "Should be an ADT:   `%s`\n%!" (GT.show t2 (A 5))

(* The [@@combinatorial] annotation asks the library to generate transformation function in
   combinatorial manner (only for type aliases). *)
type t3 = t [@@deriving gt ~options:{show}] [@@combinatorial]
let () = Format.printf "Should be a number: `%s`\n%!" (GT.show t3 (A 5))
