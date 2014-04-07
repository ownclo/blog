---
title: Byte-compiling eval-after-load
tags: emacs
published: 2013-05-01
---

<div class="alert alert-warning">

**Warning!**

If you use a recent build of Emacs trunk, be sure to read the [follow up][] on
the new Emacs macro `with-eval-after-load`.

</div>

In this post we will explore `eval-after-load` and its relation to byte
compilation.  We introduce `eval-after-load`, and point out the lack of byte
compilation as serious drawback.  Eventually we will present a macro around
`eval-after-load` which compiles `eval-after-load` forms, and explain its
advantages and disadvantages.

Introducing eval-after-load
===========================

The GNU Emacs function `eval-after-load` schedules a form for evaluation after a
named feature or a file:

```commonlisp
(eval-after-load 'ido
  '(setq ido-enable-flex-matching t      ; Match characters if string doesn't
                                         ; match
         ido-create-new-buffer 'always   ; Create a new buffer if nothing
                                         ; matches
         ido-use-filename-at-point 'guess
         ido-default-file-method 'selected-window))
```

In this example, the `setq` form is scheduled for evaluation after the feature
`ido` is loaded.

If you use `eval-after-load` in your `init.el`, Emacs will start faster, because
configuration and customization code is delayed until after the corresponding
packages are actually loaded.  Moreover, the byte compiler will not emit bogus
warnings about assignment to free variables for your `setq` forms.

Many users do not use `eval-after-load` directly, but write a helper macro
instead to allow for multiple body forms and get rid of the quoting:

```commonlisp
(defmacro stante-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FEATURE may be an unquoted feature symbol or a file name, see
`eval-after-load'."
  (declare (indent 1) (debug t))
  `(eval-after-load ',feature
     '(progn ,@forms)))

(stante-after python
  (add-hook 'python-mode-hook #'subword-mode)
  (setq python-check-command "flake8"))
```

With this macro, we do not need to quote the feature name or the form.  Moreover
we can use multiple forms at once.

Lacking byte-compilation in eval-after-load
===========================================

There is a serious drawback in using `eval-after-load`: The function does
**not** compile its body.  If you are heavily using `eval-after-load` in your
`init.el`, like I do, most of it will not be compiled actually!

Thus initialization will be somewhat slower, and even worse, you will **not** be
warned by the compiler, if you assign to free variables or call undefined
functions, which often indicates mistakes like typos in variable names.

Implementing byte compilation
=============================

This issue is reported as [bug #13021][].  While the actual discussion of this
feature has apparently dropped off, the bug report includes an implementation of
a byte-compiling `eval-after-load`:

```commonlisp
(defmacro stante-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  ;; Byte compile the body.  If the feature is not available, ignore warnings.
  ;; Taken from
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-11/msg01262.html
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (eval-after-load ',feature
      `(funcall (function ,(lambda () ,@forms))))))
```

Because this implementation is rather convoluted, we will reproduce it step by
step in the following.  pFor the sake of brevity, we will omit the docstring and
the `declare` form in subsequent definitions of the macro.  If you want to copy
this macro to your own `init.el`, be sure to use the definition above, not any
of those below!

Unveiling the implementation
============================

In our simple definition of `stante-after` above we used a standard quoted form.
The byte compiler cannot compile such forms, because they can contain arbitrary
data (i.e. quoted lists), and not just executable code.

The basic idea
--------------

We need to tell the byte compiler explicitly, that the `eval-after-load` form is
executable code, by quoting the forms with a [“function” quote][]:

> This special form returns FUNCTION-OBJECT without evaluating it.  In this, it
> is similar to `quote` […].  But unlike `quote`, it also serves as a note to
> the Emacs evaluator and byte-compiler that FUNCTION-OBJECT is intended to be
> used as a function.  Assuming FUNCTION-OBJECT is a valid lambda expression,
> this has two effects:
>
> -   When the code is byte-compiled, FUNCTION-OBJECT is compiled into a
>     byte-code function object […].
>
> […]

As `function` wants a function object, we wrap our forms into a `lambda` instead
of a `progn`:

```commonlisp
(defmacro stante-after (feature &rest forms)
  `(eval-after-load ',feature
     `(funcall (function ,(lambda () ,@forms)))))

(stante-after python
  (add-hook 'python-mode-hook #'subword-mode)
  (setq python-check-command "flake8"))
```

Now the `forms` are compiled with the surrounding code.

Avoiding bogus warnings
-----------------------

If you try this macro, you will see a lot of bogus warnings about undefined
functions or free variables during byte compilation, even though the functions
and warnings are defined in the corresponding `feature`:

    init.el:1112:9:Warning: assignment to free variable `python-check-command'

The byte compiler does simply not know about these functions and variables,
because the `feature` is never actually loaded during byte compilation.  To
avoid these warnings, we consequently need to load the feature during byte
compilation:

```commonlisp
(defmacro stante-after (feature &rest forms)
  (when (and (boundp 'byte-compile-current-file) byte-compile-current-file)
    (if (symbolp feature) (require feature) (load feature)))
  `(eval-after-load ',feature
     `(funcall (function ,(lambda () ,@forms)))))
```

The `if` form loads the feature or file with `require` and `load` respectively,
depending on the type of the `feature` argument.  Note that the `if` form is
**not** part of the macro expansion.  Thus it is **executed** during macro
expansion, instead of appearing in the expanded body.

Since macros are expanded during byte compilation, the `feature` is now loaded
during byte compilation.  The compiled code only contains the macro expansion,
so the feature is not loaded during evaluation.

We wrap the `if` form into a `when` conditional which is only entered if
`byte-compile-current-file` is non-nil, to avoid that features and libraries are
loaded if the macro is expanded at runtime, which happens if you didn't
byte-compile your init file.

Handling missing features gracefully
------------------------------------

If all features are available during byte compilation, we are done now. However,
if some of the features in your `init.el` are not available during compilation,
it got much worse now:

```commonlisp
(stante-after tex
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save
        TeX-clean-confirm nil))         ; Do not ask for confirmation when
                                        ; cleaning
```

The `tex` feature comes from [AUCTeX][].  If this library is not installed, byte
compilation brutally fails now:

    init.el:830:1:Error: Cannot open load file: tex

We obviously need to handle the missing features more gracefully:

```commonlisp
(defmacro stante-after (feature &rest forms)
  (when (and (boundp 'byte-compile-current-file) byte-compile-current-file)
    (if (symbolp feature)
        (require feature nil :no-error)
      (load feature :no-message :no-error)))
  `(eval-after-load ',feature
     `(funcall (function ,(lambda () ,@forms)))))
```

The `:no-error` argument to `load` and `require` respectively suppresses errors
if the given file or feature could not be loaded.  Actually any non-nil argument
will do, but I prefer verbose symbols for the sake of clarity.

Avoiding warnings from missing features
---------------------------------------

We are almost there now.  There is just a little nuisance left: If the feature
is not available, the byte compiler will again emit bogus warnings:

    init.el:821:9:Warning: assignment to free variable `TeX-parse-self'
    init.el:823:9:Warning: assignment to free variable `TeX-auto-save'
    init.el:824:9:Warning: assignment to free variable `TeX-clean-confirm'

We can fix this problem by wrapping the `eval-after-load` form with
`with-no-warnings`, if the feature is not available.  `with-no-warnings`
suppresses all byte compiler warnings in the contained block.  Both `load` and
`require` will return `nil` if the feature or file is not available:

```commonlisp
(defmacro stante-after (feature &rest forms)
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (eval-after-load ',feature
      `(funcall (function ,(lambda () ,@forms))))))
```

We introduce another `if` that wraps around the `eval-after-load` during
expansion, and turn our previous `when` into a `or`.  If the feature is not
available during byte compilation, that is, if the `or` returns `nil`, we print
a single warning, and wrap the `eval-after-load` with `with-not-warnings`.
Otherwise we just wrap it into a `progn` to preserve the warnings.

Now we are done.  With this macro, `eval-after-load` forms are compiled to byte
code, giving you both the speedup of byte code, and the helpful warnings and
hints of the byte compiler.

[follow up]: internal:posts/introducing-with-eval-after-load.md
[bug #13021]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13021
[“function” quote]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html#index-function-753
[auctex]: http://www.gnu.org/software/auctex/
