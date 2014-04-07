---
title: Syntax checker executables in Flycheck
tags: emacs,flycheck
published: 2013-11-26
---

In the current release of Flycheck, the executable used by a syntax checker is
hard-coded.  The `python-pylint` syntax checker will always use the first
`pylint` executable in `exec-path`.

The only way to effectively change the executable was to create symbolic links
in a directory in front of `exec-path`, which is somewhat inconvenient, if you
wanted to change the executable frequently, or use a different executable in
different buffer.

Since [cb5de77][] ([#272][]) Flycheck now provides means to change the
executable used by a syntax checker.

Changing executables interactively
==================================

To use a different executable in the current buffer, press <kbd>C-c ! e</kbd>
(`flycheck-set-checker-executable`).  The command prompts for a syntax checker
and an executable file, and uses the selected executable for the selected syntax
checker in the current buffer.

For instance, to use `pylint3` as executable for `python-pylint`, type <kbd>C-c
! e python-pylint RET pylint3</kbd>.  To reset the executable to the default,
type <kbd>C-u C-c ! e</kbd>, that is `flycheck-set-checker-executable` with
prefix argument.

The command then prompts for a syntax checker only, and resets the executable of
the selected syntax checker to the default value given in the syntax checker
definition.

Executable variables
====================

The workhorse behind this command are “executable variables”: Each syntax
checker now has a associated variable called `flycheck-CHECKER-exectuable`,
where `CHECKER` is the name of the syntax checker.  For instance, the associated
variable of the `python-pylint` syntax checker is
`flycheck-python-pylint-executable`.  These variables are buffer-local and
customizable with `M-x customize-group RET flycheck-executables`.

The value of such a variable is either `nil`, to use the default executable from
the definition of the syntax checker, or a string with the name or the path to
an executable.  For instance, the following code is equivalent to the
interactive command from the last section:

```commonlisp
(setq flycheck-python-pylint-executable "pylint3")
```

If the variable is not `nil`, its value is used as executable when running the
syntax checker.  If the value is not an absolute path to an executable, it is
search in `exec-path` using `executable-find`.

Applications
============

- As a Ruby developer, you can now use different Ruby versions to test buffers.
  For instance, you can use Ruby 2 to check buffers from your Rails project, but
  Ruby 1.8 for your new Homebrew Formula.
- As a Python developer, you can now use Pylint from your virtualenv.  If you
  use the built-in Python mode, you probably already set
  `python-shell-virtualenv-path` via directory local variables.  In this case,
  you can easily automate this with the following Emacs Lisp code:

    ```commonlisp
    (defun flycheck-python-set-executables ()
      (let ((exec-path (python-shell-calculate-exec-path)))
        (setq flycheck-python-pylint-executable (executable-find "pylint")
              flycheck-python-flake8-executable (executable-find "flake8")))
      ;; Force Flycheck mode on
      (flycheck-mode))

    (defun flycheck-python-setup ()
      (add-hook 'hack-local-variables-hook #'flyspell-python-set-executables
                nil 'local))

    (add-hook 'python-mode-hook #'flycheck-python-setup)
    ```

There are probably countless other applications for this new feature I have not
though of.  If you have done something cool with it, please let me know.

Have fun with Flycheck!

[cb5de77]: https://github.com/flycheck/flycheck/commit/cb5de77314a3cbee938a23a83b4c8a4516384388
[#272]: https://github.com/flycheck/flycheck/pull/272
