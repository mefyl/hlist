(alias (name extdeps) (deps opam/hlist.opam.extdeps))
(subdir opam
 (rule (target hlist.%{version:hlist}.opam) (deps (:opam ../hlist.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/hlist#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target hlist.opam.locked)
  (action (run %{bin:opam} lock hlist)))
 (rule (mode (promote (until-clean))) (target hlist.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:hlist.opam.locked} --local hlist))))
 (rule (target hlist-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../hlist.opam} --cross hlist --cross-both metapp --cross-exclude
     ""))))
 (rule (target hlist-ios.%{version:hlist}.opam) (deps (:opam hlist-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/hlist#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target hlist-ios.opam.locked)
  (action (run %{bin:opam} lock hlist-ios)))
 (rule (mode (promote (until-clean))) (target hlist.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:hlist-ios.opam.locked} --local hlist --toolchain ios))))
 (rule (target hlist-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../hlist.opam} --cross hlist --cross-both metapp --cross-exclude
     ""))))
 (rule (target hlist-macos.%{version:hlist}.opam)
  (deps (:opam hlist-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/hlist#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target hlist-macos.opam.locked)
  (action (run %{bin:opam} lock hlist-macos)))
 (rule (mode (promote (until-clean))) (target hlist.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:hlist-macos.opam.locked} --local hlist --toolchain macos))))
 (rule (target hlist-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../hlist.opam} --cross hlist --cross-both metapp --cross-exclude
     ""))))
 (rule (target hlist-windows.%{version:hlist}.opam)
  (deps (:opam hlist-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/hlist#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target hlist-windows.opam.locked)
  (action (run %{bin:opam} lock hlist-windows)))
 (rule (mode (promote (until-clean))) (target hlist.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:hlist-windows.opam.locked} --local hlist --toolchain windows))))
 (rule (target hlist-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../hlist.opam} --cross hlist --cross-both metapp --cross-exclude
     ""))))
 (rule (target hlist-android.%{version:hlist}.opam)
  (deps (:opam hlist-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/hlist#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target hlist-android.opam.locked)
  (action (run %{bin:opam} lock hlist-android)))
 (rule (mode (promote (until-clean))) (target hlist.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:hlist-android.opam.locked} --local hlist --toolchain android)))))
