; filepath: test/dune.inc
; Auto-generated rules to run typing_test.exe on each C file and diff
; against the corresponding .expected file.

(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/for.c)
 (action
	(with-stdout-to for.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/for.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/for.expected
	for.output)
 (action
	(diff cprogs/for.expected for.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/plus.c)
 (action
	(with-stdout-to plus.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/plus.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/plus.expected
	plus.output)
 (action
	(diff cprogs/plus.expected plus.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/while.c)
 (action
	(with-stdout-to while.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/while.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/while.expected
	while.output)
 (action
	(diff cprogs/while.expected while.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/small.c)
 (action
	(with-stdout-to small.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/small.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/small.expected
	small.output)
 (action
	(diff cprogs/small.expected small.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/switch.c)
 (action
	(with-stdout-to switch.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/switch.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/switch.expected
	switch.output)
 (action
	(diff cprogs/switch.expected switch.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/typeof.c)
 (action
	(with-stdout-to typeof.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/typeof.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/typeof.expected
	typeof.output)
 (action
	(diff cprogs/typeof.expected typeof.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/cast.c)
 (action
	(with-stdout-to cast.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/cast.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/cast.expected
	cast.output)
 (action
	(diff cprogs/cast.expected cast.output)))


(rule
 (alias runtest)
 (deps
	(glob_files ../types/*.ty)
	cprogs/narrowing_comp.c)
 (action
	(with-stdout-to narrowing_comp.output
	 (run %{exe:typing_test.exe} %{dep:cprogs/narrowing_comp.c}))))

(rule
 (alias runtest)
 (deps
	cprogs/narrowing_comp.expected
	narrowing_comp.output)
 (action
	(diff cprogs/narrowing_comp.expected narrowing_comp.output)))

  
(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/error-vs-fail.c)
 (action  (with-stdout-to error-vs-fail.output
							(run %{exe:typing_test.exe} %{dep:cprogs/error-vs-fail.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/error-vs-fail.c cprogs/error-vs-fail.expected)
 (action  (diff cprogs/error-vs-fail.expected error-vs-fail.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/timeout-body.c)
 (action  (with-stdout-to timeout-body.output
							(run %{exe:../bin/main.exe} --timeout 0.000001 %{dep:cprogs/timeout-body.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/timeout-body.expected timeout-body.output)
 (action  (diff cprogs/timeout-body.expected timeout-body.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/list.c)
 (action  (with-stdout-to list.output
							(run %{exe:typing_test.exe} %{dep:cprogs/list.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/list.c cprogs/list.expected)
 (action  (diff cprogs/list.expected list.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/string.c)
 (action  (with-stdout-to string.output
							(run %{exe:typing_test.exe} %{dep:cprogs/string.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/string.c cprogs/string.expected)
 (action  (diff cprogs/string.expected string.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/env.c)
 (action  (with-stdout-to env.output
							(run %{exe:typing_test.exe} %{dep:cprogs/env.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/env.c cprogs/env.expected)
 (action  (diff cprogs/env.expected env.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/struct.c)
 (action  (with-stdout-to struct.output
							(run %{exe:typing_test.exe} %{dep:cprogs/struct.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/struct.c cprogs/struct.expected)
 (action  (diff cprogs/struct.expected struct.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/cpatterns.c)
 (action  (with-stdout-to cpatterns.output
							(run %{exe:typing_test.exe} %{dep:cprogs/cpatterns.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/cpatterns.c cprogs/cpatterns.expected)
 (action  (diff cprogs/cpatterns.expected cpatterns.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/preproc-define.c)
 (action  (with-stdout-to preproc-define.output
							(run %{exe:typing_test.exe} %{dep:cprogs/preproc-define.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/preproc-define.c cprogs/preproc-define.expected)
 (action  (diff cprogs/preproc-define.expected preproc-define.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/preproc-ifdef.c)
 (action  (with-stdout-to preproc-ifdef.output
							(run %{exe:typing_test.exe} %{dep:cprogs/preproc-ifdef.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/preproc-ifdef.c cprogs/preproc-ifdef.expected)
 (action  (diff cprogs/preproc-ifdef.expected preproc-ifdef.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/enum.c)
 (action  (with-stdout-to enum.output
							(run %{exe:typing_test.exe} %{dep:cprogs/enum.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/enum.c cprogs/enum.expected)
 (action  (diff cprogs/enum.expected enum.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/self-referential.c)
 (action  (with-stdout-to self-referential.output
							(run %{exe:typing_test.exe} %{dep:cprogs/self-referential.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/self-referential.c cprogs/self-referential.expected)
 (action  (diff cprogs/self-referential.expected self-referential.output)))
(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/globals-probe.c)
 (action  (with-stdout-to globals-probe.output
              (run %{exe:typing_test.exe} %{dep:cprogs/globals-probe.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/globals-probe.c cprogs/globals-probe.expected)
 (action  (diff cprogs/globals-probe.expected globals-probe.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/global-write.c)
 (action  (with-stdout-to global-write.output
              (run %{exe:typing_test.exe} %{dep:cprogs/global-write.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/global-write.c cprogs/global-write.expected)
 (action  (diff cprogs/global-write.expected global-write.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/block-scope.c)
 (action  (with-stdout-to block-scope.output
              (run %{exe:typing_test.exe} %{dep:cprogs/block-scope.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/block-scope.c cprogs/block-scope.expected)
 (action  (diff cprogs/block-scope.expected block-scope.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/sexprec-alias.c)
 (action  (with-stdout-to sexprec-alias.output
              (run %{exe:typing_test.exe} %{dep:cprogs/sexprec-alias.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/sexprec-alias.c cprogs/sexprec-alias.expected)
 (action  (diff cprogs/sexprec-alias.expected sexprec-alias.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/pairlist.c)
 (action  (with-stdout-to pairlist.output
              (run %{exe:typing_test.exe} %{dep:cprogs/pairlist.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/pairlist.c cprogs/pairlist.expected)
 (action  (diff cprogs/pairlist.expected pairlist.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/r-api.c)
 (action  (with-stdout-to r-api.output
              (run %{exe:typing_test.exe} %{dep:cprogs/r-api.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/r-api.c cprogs/r-api.expected)
 (action  (diff cprogs/r-api.expected r-api.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/variadic.c)
 (action  (with-stdout-to variadic.output
              (run %{exe:typing_test.exe} %{dep:cprogs/variadic.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/variadic.c cprogs/variadic.expected)
 (action  (diff cprogs/variadic.expected variadic.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/sizeof_typedef.c)
 (action  (with-stdout-to sizeof_typedef.output
              (run %{exe:typing_test.exe} %{dep:cprogs/sizeof_typedef.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/sizeof_typedef.c cprogs/sizeof_typedef.expected)
 (action  (diff cprogs/sizeof_typedef.expected sizeof_typedef.output)))
(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/global-fnptr.c)
 (action  (with-stdout-to global-fnptr.output
              (run %{exe:typing_test.exe} %{dep:cprogs/global-fnptr.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/global-fnptr.c cprogs/global-fnptr.expected)
 (action  (diff cprogs/global-fnptr.expected global-fnptr.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/pointer.c)
 (action  (with-stdout-to pointer.output
              (run %{exe:typing_test.exe} %{dep:cprogs/pointer.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/pointer.c cprogs/pointer.expected)
 (action  (diff cprogs/pointer.expected pointer.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/const_fold.c)
 (action  (with-stdout-to const_fold.output
              (run %{exe:typing_test.exe} %{dep:cprogs/const_fold.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/const_fold.c cprogs/const_fold.expected)
 (action  (diff cprogs/const_fold.expected const_fold.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/sexp_alloc.c)
 (action  (with-stdout-to sexp_alloc.output
              (run %{exe:typing_test.exe} %{dep:cprogs/sexp_alloc.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/sexp_alloc.c cprogs/sexp_alloc.expected)
 (action  (diff cprogs/sexp_alloc.expected sexp_alloc.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/predef_macros.c)
 (action  (with-stdout-to predef_macros.output
              (run %{exe:typing_test.exe} %{dep:cprogs/predef_macros.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/predef_macros.c cprogs/predef_macros.expected)
 (action  (diff cprogs/predef_macros.expected predef_macros.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/ptr_init_conversion.c)
 (action  (with-stdout-to ptr_init_conversion.output
              (run %{exe:typing_test.exe} %{dep:cprogs/ptr_init_conversion.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/ptr_init_conversion.c cprogs/ptr_init_conversion.expected)
 (action  (diff cprogs/ptr_init_conversion.expected ptr_init_conversion.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/attributes.c)
 (action  (with-stdout-to attributes.output
              (run %{exe:typing_test.exe} %{dep:cprogs/attributes.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/attributes.c cprogs/attributes.expected)
 (action  (diff cprogs/attributes.expected attributes.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/inherits.c)
 (action  (with-stdout-to inherits.output
              (run %{exe:typing_test.exe} %{dep:cprogs/inherits.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/inherits.c cprogs/inherits.expected)
 (action  (diff cprogs/inherits.expected inherits.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/macro-namespace.c)
 (action  (with-stdout-to macro-namespace.output
              (run %{exe:typing_test.exe} %{dep:cprogs/macro-namespace.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/macro-namespace.c cprogs/macro-namespace.expected)
 (action  (diff cprogs/macro-namespace.expected macro-namespace.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/noreturn-macro.c)
 (action  (with-stdout-to noreturn-macro.output
              (run %{exe:typing_test.exe} %{dep:cprogs/noreturn-macro.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/noreturn-macro.c cprogs/noreturn-macro.expected)
 (action  (diff cprogs/noreturn-macro.expected noreturn-macro.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/truthiness.c)
 (action  (with-stdout-to truthiness.output
              (run %{exe:typing_test.exe} %{dep:cprogs/truthiness.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/truthiness.c cprogs/truthiness.expected)
 (action  (diff cprogs/truthiness.expected truthiness.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/macro-bodies.c)
 (action  (with-stdout-to macro-bodies.output
              (run %{exe:typing_test.exe} %{dep:cprogs/macro-bodies.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/macro-bodies.c cprogs/macro-bodies.expected)
 (action  (diff cprogs/macro-bodies.expected macro-bodies.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/undeclared-typedefs.c)
 (action  (with-stdout-to undeclared-typedefs.output
              (run %{exe:typing_test.exe} %{dep:cprogs/undeclared-typedefs.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/undeclared-typedefs.c cprogs/undeclared-typedefs.expected)
 (action  (diff cprogs/undeclared-typedefs.expected undeclared-typedefs.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/flow.c)
 (action  (with-stdout-to flow.output
              (run %{exe:typing_test.exe} %{dep:cprogs/flow.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/flow.c cprogs/flow.expected)
 (action  (diff cprogs/flow.expected flow.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/goto.c)
 (action  (with-stdout-to goto.output
              (run %{exe:typing_test.exe} %{dep:cprogs/goto.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/goto.c cprogs/goto.expected)
 (action  (diff cprogs/goto.expected goto.output)))

(rule
 (alias   runtest)
 (deps    (glob_files ../types/*.ty) cprogs/empty-statement.c)
 (action  (with-stdout-to empty-statement.output
              (run %{exe:typing_test.exe} %{dep:cprogs/empty-statement.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/empty-statement.c cprogs/empty-statement.expected)
 (action  (diff cprogs/empty-statement.expected empty-statement.output)))

