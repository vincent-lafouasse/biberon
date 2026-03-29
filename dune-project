(lang dune 3.19)

(name biberon)

(generate_opam_files true)

(source
 (github vincent-lafouasse/no-repo-yet))

(authors "poss (vincent lafouasse)")

(maintainers "poss (vincent lafouasse)")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name biberon)
 (synopsis "a citation formatter")
 (description "generate markdown from a .bib")
 (depends ocaml)
 (tags
  (bibtex markdown citation bibliography)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
