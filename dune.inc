(rule (target hlist.%{version:hlist}.opam) (deps (:opam hlist.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/hlist#%{version:hlist}\" }")))))
(rule (target hlist-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:hlist.opam} --cross hlist --cross-both metapp --cross-exclude ""))))
(rule (target hlist-ios.%{version:hlist}.opam) (deps (:opam hlist-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/hlist#%{version:hlist}\" }")))))
(rule (deps (universe)) (target hlist.opam.locked)
 (action (run %{bin:opam} lock hlist)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target hlist.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:hlist.opam.locked} --local hlist))))
