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
  (depends
    (gettext (= :version))
    (dune-configurator (>= 3.17.0))
    (ounit2 (and (>= 2.2.7) :with-test))))

(package
  (name gettext-camomile)
  (depends
    (gettext (= :version))
    (camomile (<= 2.0.0))
    (ounit2 (and (>= 2.2.7) :with-test))
    (fileutils (and (>= 0.6.6) :with-test))))
