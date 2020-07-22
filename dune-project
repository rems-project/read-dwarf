(lang dune 2.4)
(name "read-dwarf")
(authors "Peter Sewell" "Thibaut Pérami")
(maintainers "Peter Sewell <peter.sewell@cl.cam.ac.uk>"
             "Thibaut Pérami <thibaut.perami@cl.cam.ac.uk>")
(source (github rems-project/read-dwarf))

(using menhir 2.0)
(generate_opam_files true)
(wrapped_executables false)

(package
 (name read-dwarf)
 (synopsis "C Translation checking software")
 (depends
  (ocaml (>= 4.08.0))
  (pprint (>= 20171003))
  (menhir :build)
  (linksem (>= 0.7))
  (cmdliner (>= 1.0.0))
  (res (>= 5.0.0))
  (ocamlgraph (>= 1.8.8))
  (toml (>= 5.0.0))
  isla-lang
  (ott (and (>= 0.31) :build))
  (qtest :with-test)
  (ounit :with-test)
  (odoc :with-doc)
 )

)
