[![GT][1]][2]
[![GT][3]][4]
[![License](https://img.shields.io/badge/license-LGPL-blue)](https://github.com/PLTools/GT/blob/master/COPYING)
[![API docs](https://img.shields.io/badge/Documentation-API-yellowgreen)](https://PLTools.github.io/GT/GT)

[1]:  https://github.com/PLTools/GT/actions/workflows/master.yml/badge.svg?branch=master
[2]:  https://github.com/PLTools/GT/actions/workflows/master.yml
[3]:  https://github.com/PLTools/GT/actions/workflows/master52.yml/badge.svg?branch=master
[4]:  https://github.com/PLTools/GT/actions/workflows/master52.yml

[![Coverage Status](https://coveralls.io/repos/github/PLTools/GT/badge.svg?branch=master)](https://coveralls.io/github/PLTools/GT?branch=master)

# Datatype-generic object-oriented transformations for OCaml (a.k.a. GT)

This library implements a framework for datatype-generic programming in Objective Caml language.

The key feature of the approach in question is object-oriented representation of transformations performed over regular algebraic datatypes. Our implementation supports polymorphic variants; in particular, a transformation for a "joined" polymorphic variant type can be acquired via inheritance from the transformations for its counterparts.

### See also

[visitors](https://gitlab.inria.fr/fpottier/visitors)

[BAP's vistors](http://binaryanalysisplatform.github.io/bap/api/master/Bap.Std.Exp.visitor-c.html)

[Janestreet's PPX Traverse](https://github.com/janestreet-deprecated/ppx_traverse)

## Installation

    opam pin add GT https://github.com/PLTools/GT.git -y

  or from the main opam repository

    opam update
    opam install GT -y

## Usage

### As PPX

Use findlib package `GT.ppx` in combination with `ppxlib`. See `ppxlib`'s manual for full guidance. In short do

```
~ ocaml
OCaml version 4.14.1
Enter #help;; for help.

# #use "topfind";;
# #require "GT";;
# #require "GT.ppx_all";;
../GT/ppx_all: added to search path
../GT/ppx_all/./ppx.exe --as-ppx: activated
# type 'a list = Nil | Cons of 'a * 'a list [@@deriving gt ~options:{fmt; show}];;
...
```

### As Camlp5 syntax extension

Use findlib package `GT.syntax.all` to enable extension and all built-in plugins. To compile and see the generated code use the following command:

    ocamlfind opt -syntax camlp5o -package GT.syntax.all regression/test081llist.ml -dsource

To preprocess only the code in this library (for example, a test) use the following shell command:

    dune exec camlp5/pp5+gt+plugins+o.exe regression/test005.ml

To use camlp5 (>= 7.12) syntax extension in toplevel try (after installation) this:

    #use "topfind.camlp5";;
    #camlp5o;;
    #require "GT,GT.syntax,GT.syntax.show,GT.syntax.map";;
    @type t = GT.int with gmap,show;; (* for example *)

## Directory structure

- The framework for generation is in `common/`. The generic plugin for adding new transformations is in `common/plugin.ml`.
- All built-in plugins live in `plugins/` and depend on the stuff in `common/`.
- Camlp5-specific preprocessing plugin lives in `camlp5/`. Depend on stuff in `common/`.
- PPX-specific preprocessing plugin lives in `ppx/`. Depends on stuff in `common/`.
- Built-in plugins that represent transformations live in `plugins/`. Depends on `common/`.
- A library for built-in types and transformations for types from Pervasives live in `src/`. Depends on syntax extension from `camlp5/` and plugins from `plugins/`.

# Dependencies

- `ppxlib`
- `camlp5
- `ocamlgraph` for topological sorting
- `ocamlbuild` as build system

# Compilation

- `make` to compile whole library.
- `make && make tests` to compile regression tests too.

In case some of the tests do not compile use following commands to see generated code:

- with camlp5 use `dune exec camlp5/pp5+gt+plugins+o.exe regression/test817logic.ml`
- with PPX use `dune exec ppx/pp_gt.exe regression/test801mutal.ml`

To build documentation set the environment variable `GT_WITH_DOCS` and run `opam install odoc --yes && dune build @doc`.
The generated HTML files will be located at `_build/default/_doc/_html/index.html`.

In the following section we describe our approach in a nutshell by a typical example.

## Limitations

Known to be not supported or not taken to account:

- non-regular recursive types
- GADTs

## TODO

Can be a bug:

- Method `on_record_declaration` doesn't introduce new pattern names systematically
- For `compare` and `eq` plugins in case of ADT with single constructor we generate unreachable pattern matching pattern that gives a warning.

Improvements:
- Documentation for `src/GT.ml` is not generated (possible because of a macro).
- Better signature for `method virtual on_record_constr`.
- Custom transformation functions for type parameters has become broken after introducing combinatorial interface for type abbreviations.
- Allow `[@@named "..."]` attribute to provide a custom name for non-latin constructors (like lists).
- Sometimes we need override class definition for a plugin. It should be possible to specify new custom class inside the attribute.

## References

- Dmitry Kosarev, Dmitry Boulytchev. [Generic Programming with Combinators and Objects](https://arxiv.org/pdf/2106.01250.pdf) // submitted to ML workshop 2018
- Dmitry Boulytchev. [Code Reuse with Object-Encoded Transformers](https://oops.math.spbu.ru/papers/transformation-objects-talk.pdf) // A talk at the International Symposium on Trends in Functional Programming, 2014.
- Dmitry Boulytchev. [Code Reuse with Transformation Objects](https://arxiv.org/pdf/1802.01930.pdf) // unpublished.
- Dmitry Boulytchev. [Combinators and Type-Driven Transformers in Objective Caml](https://www.sciencedirect.com/science/article/pii/S0167642315001422) // submitted to the Science of Computer Programming.
