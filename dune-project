(lang dune 3.22)

(name map)

(generate_opam_files true)

(cram enable)

(source
 (codeberg debarchito/map))

(license GPL-3.0-only)

(authors "Debarchito Nath <debarchiton@proton.me>")

(maintainers "Debarchito Nath <debarchiton@proton.me>")

(package
 (name map)
 (description "A visualizable VM")
 (depends
  (ocaml
   (>= "5.4.1"))
  (raylib
   (>= "1.6.0"))))

(package
 (name fold)
 (description "A simple lisp than run on top of the map VM")
 (depends
  (ocaml
   (>= "5.4.1"))
  map
  (menhir
   (>= "20260209"))
  (raylib
   (>= "1.6.0"))))

(using menhir 3.0)
