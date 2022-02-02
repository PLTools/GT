(lang dune 2.8)

(generate_opam_files true)

(cram enable)

(name GT)

(package
 (name GT-p5))

(package
 (name GT)
 (synopsis "Generic programming with extensible transformations")
 (description
  "Yet another library for generic programming. Provides syntax extensions\nboth for camlp5 and PPX which allow decoration of type declarations with\nfollowing compile-time code generation. Provides the way for creating\nplugins (compiled separately from the library) for enchancing supported\ntype transformations.\n\nStrongly reminds the `visitors` library from Fran\195\167ois Pottier.\nDuring desing of a library of these kind there many possible\ndesign decision and in many cases we decided to implement\nthe decision opposite to the one used in `visitors`.")
 (authors "https://github.com/dboulytchev" "https://github.com/Kakadu")
 (maintainers "Kakadu@pm.me")
 (homepage "https://github.com/JetBrains-Research/GT")
 (bug_reports "https://github.com/JetBrains-Research/GT/issues")
 (version 0.5)
 (depends
  dune
  (ppxlib
   (<= "0.25.0"))
  ocamlfind
  ocamlgraph
  logger-p5
  ppx_inline_test
  (ocaml-migrate-parsetree
   (>= "2.1.0"))
  (camlp5
   (>= "8.00.01"))
  (conf-m4 :build)
  base))

(package
 (name GT-bench)
 (synopsis "Some benchmarks. Should not be installed")
 (version 0.1)
 (depends dune benchmark))
