(lang dune 3.2)

(name lox_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lox_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   alcotest)
   
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
