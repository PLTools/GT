(lang dune 2.7)

(generate_opam_files true)

(cram enable)

(name GT)

(allow_approximate_merlin)

(package
 (name GT)
 (synopsis "Generic programming with extensible transformations")
 (description "Yet another library for generic programming. Provides syntax extensions
both for camlp5 and PPX which allow decoration of type declarations with
following compile-time code generation. Provides the way for creating
plugins (compiled separately from the library) for enchancing supported
type transformations.

Strongly reminds the `visitors` library from Fran\195\167ois Pottier.
During desing of a library of these kind there many possible
design decision and in many cases we decided to implement
the decision opposite to the one used in `visitors`.")
 (authors "https://github.com/dboulytchev" "https://github.com/Kakadu")
 (maintainers "Kakadu@pm.me")
 (homepage "https://github.com/JetBrains-Research/GT")
 (bug_reports "https://github.com/JetBrains-Research/GT/issues")
 (version 0.4.2)
 (depends
  dune
  ppxlib
  ocamlfind
  ocamlgraph
  logger-p5
  ppx_inline_test
  (ocaml-migrate-parsetree (>= "2.1.0"))
  (camlp5 (>= "8.00.01"))
  (conf-m4 :build)
  base))

(package
 (name GT-p5)
 (synopsis "Camlp5 extension for generic programming library GT")
 (authors "https://github.com/dboulytchev" "https://github.com/Kakadu")
 (maintainers "Kakadu@pm.me")
 (homepage "https://github.com/JetBrains-Research/GT")
 (bug_reports "https://github.com/JetBrains-Research/GT/issues")
 (version 0.4.2)
 (depends
  dune
  ppxlib
  (ocamlfind :build)
  (camlp5
   (>= "8.00.01"))))
