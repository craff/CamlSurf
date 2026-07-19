(lang dune 3.23)

(name CamlSurf)

(generate_opam_files true)

(source
 (github craff/CamlSurf))

(authors "Christophe Raffalli <christophe@raffalli.eu>")

(maintainers  "Christophe Raffalli <christophe@raffalli.eu>")

(license MIT)

(documentation https://raffalli.eu/CamlSurf)

(package
 (name CamlSurf)
 (synopsis "A program to display and animate implicit surfaces and curves")
 (description "A program to display and animate implicit surfaces and curves")
 (depends ocaml)
 (tags
  ("surface" "curve" "geometry" "OpenGL")))
