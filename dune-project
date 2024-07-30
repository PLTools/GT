(lang dune 2.8)

(generate_opam_files true)

(cram enable)

(name GT)

(license LGPL-2.1-or-later)

(bug_reports "https://github.com/PLTools/GT/issues")

(homepage "https://github.com/PLTools/GT")

(source
 (github PLTools/GT))

(package
 (name GT)
 (synopsis "Generic programming with extensible transformations")
 (description
  "Yet another library for generic programming. Provides syntax extensions\nboth for camlp5 and PPX which allow decoration of type declarations with\nfollowing compile-time code generation. Provides the way for creating\nplugins (compiled separately from the library) for enchancing supported\ntype transformations.\n\nStrongly reminds the `visitors` library from François Pottier.\nDuring desing of a library of these kind there many possible\ndesign decision and in many cases we decided to implement\nthe decision opposite to the one used in `visitors`.\n\n\nP.S. Since 2023 development team is no longer associated with JetBrains Research")
 (authors "https://github.com/dboulytchev" "https://github.com/Kakadu")
 (maintainers "Kakadu@pm.me")
 (version 0.5.3)
 (depends
  (ppxlib
   (<= "0.32.1"))
  (ocaml
   (or
    (and
     (>= "4.14")
     (< "5.0.0"))
    (and
     (>= "5.2.0")
     (< "5.3.0"))))
  (camlp5
   (>= "8.00.05"))
  dune
  ocamlgraph
  ppx_inline_test
  (ocamlfind :build)
  (logger-p5 :build)
  (bisect_ppx :build)
  (conf-m4 :build)
  (odoc :with-doc)
  (odig :with-doc)
  (pa_ppx :with-doc)
  ;
  ))

(package
 (name GT-bench)
 (synopsis "Some benchmarks. Should not be installed")
 (version 0.1)
 (authors "Dmitrii Kosarev a.k.a. Kakadu")
 (maintainers "Kakadu@pm.me")
 (depends dune benchmark))
