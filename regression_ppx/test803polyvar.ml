open GT

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  [@@deriving gt ~show ~gmap]
end = struct
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  [@@deriving gt ~show ~gmap]
end

let _ =
  let open PV in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv id             id (`A "1"))
      (show_pv string_of_int  id @@
       gmap_pv int_of_string  id  (`A "1"))

module PVExt : sig
  type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a]
  [@@deriving gt ~show ~gmap]
end = struct
  type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a]
  [@@deriving gt ~show ~gmap]
end

let _ =
  let open PV in
  let open PVExt in
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv: %s\n" @@
      show_pv  id   id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int id
      (gmap_pv  int_of_string id (`A "1") );
  Printf.printf "Original pv_ext: %s\n" @@
    show_pv_ext  id id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int id
      (gmap_pv_ext int_of_string  id (`C "1"));

module PVExt2 : sig
  type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
  [@@deriving gt ~show ~gmap]
end = struct
  type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
  [@@deriving gt ~show ~gmap]
end

let () =
  let open PVExt in
  let open PVExt2 in

  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext: %s\n" @@
      show_pv_ext2  id id (`C "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
    show_pv_ext2  string_of_int  id
      (gmap_pv_ext  int_of_string  id (`C "1") );
  Printf.printf "Original pv_ext2: %s\n" @@
    show_pv_ext2   id id (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
    show_pv_ext2  string_of_int id
      (gmap_pv_ext2  int_of_string id (`D "1"));

module PVExt3 : sig
  type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c]
  [@@deriving gt ~show ~gmap]
end = struct
  type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c]
  [@@deriving gt ~show ~gmap]
end

let () =
  let open PVExt2 in
  let open PVExt3 in

  (* let (_:int) = show_pv_ext3 in *)
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext2: %s\n" @@
      show_pv_ext2  id id (`D "1");
  Printf.printf "Mapped pv_ext2 and showed as a pv_ext3: %s\n" @@
    show_pv_ext3  string_of_int id id @@
    gmap_pv_ext2  int_of_string id       (`D "1");
  Printf.printf "Original pv_ext3: %s\n" @@
    show_pv_ext3   id id id (`E "1");
  Printf.printf "Mapped PV_ext3 and showed as a pv_ext3: %s\n" @@
    show_pv_ext3 id  id string_of_float @@
    gmap_pv_ext3 id  id float_of_string (`E "1.0");
