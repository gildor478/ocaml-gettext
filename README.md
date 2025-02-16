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

In all the examples, we are using shorthands function `s_`, `sn_`, `f_` and
`fn_`. It is important to keep this precise names because they are used to
extract strings for translation.

This [library](examples/library) demonstrates how to define i18n module for
a library:

* Define a module [LibraryGettext](examples/library/libraryGettext.ml) that
  defines the `textdomain` and `dependencies` of the module. In this case
  `mydomain` for the `textdomain` and the only dependencies are `Gettext.init`.
* In the main library module [Library](examples/library/library.ml):
  * Use  `open LibraryGettext.Gettext` to give access to all the shorthands.
  * Make an `init` public, to allow user of the library to initialize i18n for
    this library. This `init` will be used as a `dependencies` for other
    module.

> [!NOTE]
> A library doesn't have a real implementation for gettext, but the `init`
> defines what implementation for gettext will be used at executable level.
> Typically an executable will either use `gettext-camomile` or `gettext-stub`
> and all the `init` exposed by libraries to initialize the real
> implementation.

The [GUI](examples/gui) example is very similar to the
[library](examples/library). The example is library.

This [program](examples/program) demonstrates how to define i18n module and a
concrete implementation for gettext:

* Define a module [ProgramGettext](examples/program/programGettext.ml) that
  defines:
  * a `textdomain` in this case `mydomain`
  * a list of `dependencies` in this case `ExamplesLibrary.Library.init` and
    `ExamplesGUI.Gui.init`.
  * a real implementation in this case `GettextCamomile.Map`.
* In the main module [Program](examples/program/program.ml):
  * Use `open ProgramGettext.Gettext` to give access to all the shorthands.
  * Call `ProgramGettext.Gettext.init`, use `Arg.parse` with `gettext_args`
    returned by this function.

The last pieces needed are the string files. They are traditionally stored in a
[po directory](examples/po) with a specific Makefile:

* The [Makefile](examples/po/Makefile) contains the most common targets.
* The [POTFILES](examples/po/POTFILES) defines which files to look for
  translatable strings.
* The [LINGUAS](examples/po/LINGUAS) defines what language file (`.po`) are
  present.
* Running `make all` will generate a `.pot` file and compile the various
  language files.

To test the examples:

```shell
# Update the PO/POT files.
$> make update-po

# Define location of the .mo files.
$> export OCAML_LOCALEPATH=./_build/share/locale

# Run the default program.
$> dune exec --workspace dune-workspace.dev examples/program/program.exe -- --my-name Chet

Hello world!
Hello world!
There is 1 plate.
There are 2 plates.
There is 1 plate.
There are 2 plates.
Hello Chet

# Run the program for french language.
$> LANGUAGE=fr dune exec --workspace dune-workspace.dev examples/program/program.exe -- --my-name Chet

Bonjour le monde !
Bonjour world !
Il y a 1 assiette.
Il y a 2 assiettes.
Il y a 1 assiette.
Il y a 2 assiettes.
Bonjour Chet

```
