(lang dune 2.8)
(name ppx_viewpattern)
(generate_opam_files true)
(source (github sim642/ppx_viewpattern))
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")
(license MIT)

(package
 (name ppx_viewpattern)
 (synopsis "View patterns in OCaml")
 (description "Transformation for view patterns in OCaml. Attempts to imitate Haskell view patterns.")
 (depends
  (ppxlib (>= 0.17.0))
  (ounit2 :with-test)
  (qcheck-ounit :with-test)))
