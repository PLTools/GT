(lang dune 2.8)

(generate_opam_files true)

(cram enable)

(name GT)

(source
 (github PLTools/GT))

(package
 (name GT)
 (synopsis "Generic programming with extensible transformations")
 (license LGPL-2.1-or-later)
 (description
  "Yet another library for generic programming. Provides syntax extensions\nboth for camlp5 and PPX which allow decoration of type declarations with\nfollowing compile-time code generation. Provides the way for creating\nplugins (compiled separately from the library) for enchancing supported\ntype transformations.\n\nStrongly reminds the `visitors` library from Fran\195\167ois Pottier.\nDuring desing of a library of these kind there many possible\ndesign decision and in many cases we decided to implement\nthe decision opposite to the one used in `visitors`.\n\n\nP.S. Since 2023 development team is no longer associated with JetBrains Research")
 (authors "https://github.com/dboulytchev" "https://github.com/Kakadu")
 (maintainers "Kakadu@pm.me")
 (homepage "https://github.com/PLTools/GT")
 (bug_reports "https://github.com/PLTools/GT/issues")
 (version 0.5.2)
 (depends
  (ppxlib
   (<= "0.28.0"))
  (ocaml
   (>= "4.13"))
  (camlp5
   (>= "8.00.02"))
  dune
  (ocamlfind :build)
  (logger-p5 :build)
  (bisect_ppx :build)
  (conf-m4 :build)
  ocamlgraph
  ppx_inline_test
  ;
  ))

(package
 (name GT-bench)
 (synopsis "Some benchmarks. Should not be installed")
 (version 0.1)
 (maintainers "Kakadu@pm.me")
 (depends dune benchmark))
