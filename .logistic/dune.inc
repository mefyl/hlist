(dirs :standard .logistic)

(env
 (dev
  (flags
   (:standard -warn-error -A -warn-error +partial-match)))
 (release
  (flags
   (:standard -warn-error +A -w +unused-constructor))))

(rule
 (target dune.inc)
 (action
  (with-stdout-to
   %{target}
   (run
    %{dep:.logistic/ci/dune/dune_sak.exe}
    generate
    %{read-strings:cross.flags})))
 (deps dune-project .logistic/dune.inc))

(subdir
 opam
 (dynamic_include ../dune.inc))
