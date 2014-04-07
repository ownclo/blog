---
title: Introducing with-eval-after-load
tags: emacs
published: 2013-06-25
---

At June, 13th Emacs trunk introduced a new macro with-eval-after-load in
[r112976][] (Git mirror).  It behaves like `eval-after-load`, except that it
takes multiple unquoted forms and wraps them into a `lambda` to enable byte
compilation:

```commonlisp
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'subword-mode)
  (setq python-check-command "flake8"))
```

This supersedes much of my [last post][] about byte compilation in
`eval-after-load`.  However, the new macro does not load the corresponding
features during byte compilation, so I'll wrap my old `stante-after` macro
around it to avoid bogus warnings, just like in the last post:

```commonlisp
(defmacro stante-after (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))
```

To ensure compatibility with releases and older snapshot builds, I define
`with-eval-after-load` if it is absent:

```commonlisp
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))
```

This compatibility definition of `with-eval-after-load` uses the same idea as
the old definition of the `stante-after` macro and simply wraps the body in a
`lambda` form.

[r112976]: http://git.savannah.gnu.org/cgit/emacs.git/commit/?h=trunk&id=fde7048a0bf523e22dd6d80f170c8dd380c1807e
[last post]: internal:posts/byte-compiling-eval-after-load.md
