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
 (allow_empty)
 (depends ocaml
   (gles3 (>= 20260725))
   (gles3-x11 (>= 20260725))
   (pacomb (>= 1.4.4)))
 (tags
  ("surface" "curve" "geometry" "OpenGL")))

(package
 (name CamlSurf-wayland)
 (synopsis "A program to display and animate implicit surfaces and curves")
 (description "A program to display and animate implicit surfaces and curves")
 (allow_empty)
 (depends ocaml
   (gles3 (>= 20260725))
   (gles3-wayland (>= 20260725))
   (pacomb (>= 1.4.4)))
 (tags
  ("surface" "curve" "geometry" "OpenGL")))
