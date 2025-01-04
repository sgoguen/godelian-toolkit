(lang dune 3.0)
(name godelian_toolkit)
(generate_opam_files true)
(package
 (name godelian_toolkit)
 (allow_empty)
 (depends
  (ocaml (>= 4.14.0))
  dune
  zarith))