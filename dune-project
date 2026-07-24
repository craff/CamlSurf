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
 (name CamlSurf-x11)
 (synopsis "A program to display and animate implicit surfaces and curves")
 (description "A program to display and animate implicit surfaces and curves")
 (depends ocaml gles3 gles3-x11 pacomb)
 (tags
  ("surface" "curve" "geometry" "OpenGL")))

(package
 (name CamlSurf-wayland)
 (synopsis "A program to display and animate implicit surfaces and curves")
 (description "A program to display and animate implicit surfaces and curves")
 (depends ocaml gles3 gles3-wayland pacomb)
 (tags
  ("surface" "curve" "geometry" "OpenGL")))
