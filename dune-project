(lang dune 3.7)

(using menhir 2.1)

(name language_server)

(generate_opam_files true)

(source
 (github is-hoku/yacc-language-server))

(authors "Hoku Ishibe")

(maintainers "Hoku Ishibe")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name language_server)
 (synopsis "Language Server for Yacc/Bison")
 (description "Language Server for Yacc/Bison")
 (depends ocaml dune)
)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
