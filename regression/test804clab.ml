type 'a t = C of {xxx: 'a; yyy: GT.int}
[@@deriving gt ~options:{ html; fmt }]

let () =
  let x1 =  (C {xxx="asdf"; yyy=1}) in
  fmt_t GT.string.plugins#fmt Format.std_formatter x1;
  Format.printf "%s" @@ HTML.toHTML @@
  html_t GT.string.plugins#html x1;
  ()

