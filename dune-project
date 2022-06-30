(lang dune 3.3)

(name warc)

(generate_opam_files true)

(source
 (github zshipko/ocaml-warc))

(authors "Zach Shipko")

(maintainers "Zach Shipko")

(license ISC)

(documentation https://github.com/zshipko/ocaml-warc)

(package
 (name warc)
 (synopsis "WARC format reader/writer")
 (description "A library for reading and writing Web ARChive (.warc) files")
 (depends ocaml dune uuidm ISO8601)
 (tags
  (warc web-archive)))

