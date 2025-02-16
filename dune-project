(lang dune 3.17)
(name gettext)

(explicit_js_mode)
(using dune_site 0.1)
(generate_opam_files)

(source (github gildor478/ocaml-gettext))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(authors "Sylvain Le Gall")
(maintainers "Sylvain Le Gall <sylvain+ocaml@le-gall.net>")
(documentation "https://gildor478.github.io/ocaml-gettext/")

(package
  (name gettext)
  (synopsis "Internationalization library (i18n)")
  (description
"\| This library enables string translation in OCaml. The API is based on GNU
"\| gettext. It comes with a tool to extract strings which need to be translated
"\| from OCaml source files.
"\|
"\| This enables OCaml program to output string in the native language of
"\| the user, if a corresponding translation file of the English strings is
"\| provided.
"\|
 )
  (depends
    dune-site
    (ocaml (>= "4.14.0"))
    (cppo (and (>= 1.8.0) :build))
    (seq (and (>= "base") :with-test))
    (ounit2 (and (>= 2.2.7) :with-test))
    (fileutils (>= 0.6.6)))
  (sites (share locale)))

(package
  (name gettext-stub)
  (synopsis "Internationalization using C gettext library (i18n)")
  (description "See gettext package description.")
  (depends
    (gettext (= :version))
    (dune-configurator (>= 3.17.0))
    (ounit2 (and (>= 2.2.7) :with-test))))

(package
  (name gettext-camomile)
  (synopsis "Internationalization library using camomile (i18n)")
  (description "See gettext package description.")
  (depends
    (gettext (= :version))
    (camomile (<= 2.0.0))
    (ounit2 (and (>= 2.2.7) :with-test))
    (fileutils (and (>= 0.6.6) :with-test))))
