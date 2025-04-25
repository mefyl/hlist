(lang dune 3.14)

(name hlist)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.2.2)

(homepage https://github.com/mefyl/hlist)

(bug_reports https://github.com/routine/hlist/-/issues)

(source
 (github mefyl/hlist))

(package
 (name hlist)
 (synopsis "Heterogeneous lists")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (ocaml
   (>= 4.12.0))
  (ppx_here :with-test)))
