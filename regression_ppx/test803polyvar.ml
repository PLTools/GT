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
      (show_pv (GT.lift id)             (GT.lift id) () (`A "1"))
      (show_pv (GT.lift string_of_int)  (GT.lift id) () @@
       gmap_pv (GT.lift int_of_string)  (GT.lift id) () (`A "1"))

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
      show_pv  (GT.lift id)  (GT.lift id) () (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
    show_pv_ext (GT.lift string_of_int)  (GT.lift id) ()
      (gmap_pv  (GT.lift int_of_string)  (GT.lift id) () (`A "1") );
  Printf.printf "Original pv_ext: %s\n" @@
    show_pv_ext  (GT.lift id)  (GT.lift id) ()  (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    show_pv_ext (GT.lift string_of_int)  (GT.lift id) ()
      (gmap_pv_ext (GT.lift int_of_string)  (GT.lift id) () (`C "1"));

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
      show_pv_ext2  (GT.lift id)  (GT.lift id) () (`C "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
    show_pv_ext2  (GT.lift string_of_int)  (GT.lift id) ()
      (gmap_pv_ext  (GT.lift int_of_string)  (GT.lift id) () (`C "1") );
  Printf.printf "Original pv_ext2: %s\n" @@
    show_pv_ext2   (GT.lift id)  (GT.lift id) () (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
    show_pv_ext2  (GT.lift string_of_int)  (GT.lift id) ()
      (gmap_pv_ext2  (GT.lift int_of_string)  (GT.lift id) () (`D "1"));

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
      show_pv_ext2  (GT.lift id)  (GT.lift id) () (`D "1");
  Printf.printf "Mapped pv_ext2 and showed as a pv_ext3: %s\n" @@
    show_pv_ext3  (GT.lift string_of_int) (GT.lift id) (GT.lift id) () @@
    gmap_pv_ext2  (GT.lift int_of_string) (GT.lift id)              () (`D "1");
  Printf.printf "Original pv_ext3: %s\n" @@
    show_pv_ext3   (GT.lift id) (GT.lift id) (GT.lift id) () (`E "1");
  Printf.printf "Mapped PV_ext3 and showed as a pv_ext3: %s\n" @@
    show_pv_ext3 (GT.lift id)  (GT.lift id) (GT.lift string_of_float) () @@
    gmap_pv_ext3 (GT.lift id)  (GT.lift id) (GT.lift float_of_string) () (`E "1.0");
