OCaml-gettext - Internationalization library for OCaml (i18n)
=============================================================

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/gildor478/ocaml-gettext/master&logo=ocaml)](https://ci.ocamllabs.io/github/gildor478/ocaml-gettext)

Internationalization of a program means that the program have the possibility
to handle different language. It can output messages which depend on the
language of the user.  Typically, if a program can output "bonjour" for a
french user, and "hello" for an english user, this program is
internationalized.

GNU gettext is one of the standard solutions for i18n. You just need to use
special functions to translate strings. These functions are used when the
program is running to do the translation and when compiling the program to
extract the strings automatically. In ocaml-gettext these functions are "s_",
"f_","sn_" and "fn_". They are both used to translate at runtime and to extract
strings for translation.

ocaml-gettext provides enough service to build a internationalized program. It
comes with :

* a pure Ocaml implementation, based on Camomile,
* an alternative implementation with a binding to GNU gettext library,
* `ocaml-gettext` a tool to extract strings from Ocaml source.

Installation
------------

The recommended way to install ocaml-gettext is via the [opam package manager][opam]:

```sh
$ opam install gettext gettext-camomile gettext-stub
```

Building without Camomile
-------------------------

This project uses standard `dune` mechanism for building. It provides 3
different packages:

- `gettext`: the base module
- `gettext-camomile`; gettext using Camomile.
- `gettext-stub`: gettext using the C library.

If you want to not use Camomile, you can just compile or test the other module,
using `dune`:

```sh
$ dune test --only-packages=gettext,gettext-stub
```

Documentation
-------------

* API documentation is
  [available online](https://gildor478.github.io/ocaml-gettext).
* [Reference manual](doc/reference-manual.md)

Examples
--------

* A [library](examples/library)
* A [program](examples/program)
* A [GUI](examples/gui)
