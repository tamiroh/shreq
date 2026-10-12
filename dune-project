(lang dune 3.21)

(name shreq)

(generate_opam_files true)

(source
 (github tamiroh/shreq))

(authors "tamiroh")

(maintainers "tamiroh <tamiroh71@gmail.com>")

(license MIT)

(package
 (name shreq)
 (synopsis "Check shell script environment requirements")
 (depends (ocaml (>= 4.13))))
