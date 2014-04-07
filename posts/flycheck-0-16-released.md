---
title: Flycheck 0.16 released
tags: emacs,flycheck
published: 2014-02-12
---

Time for a new release of [Flycheck][], the modern syntax-checking extension for
Emacs:

<div class="panel panel-default text-center">
<div class="panel-body">
<img src="/images/flycheck-0.16.png" class="img-responsive img-thumbnail">
</div>
<div class="panel-footer">
Flycheck 0.16 with [Solarized Light][] and [Source Code Pro][]
</div>
</div>

In two months since the last release, Flycheck got a bunch of new syntax
checkers, a brand-new error list, and the ability to override the executables of
syntax checkers.

Let's go through the list of important changes.  For a detailed list, please
read the [changelog][] in the manual.

Breaking changes
================

- The Hdevtools syntax checker was removed from Flycheck into a separate package
  [flycheck-hdevtools][] due to various issues ([#275][]).
- Support for coffeelint 0.x is dropped.

Syntax checkers
===============

New languages and checkers
--------------------------

Flycheck supports *eight* new languages: [AsciiDoc][], [Cfengine][], [Chef][]
recipes, [ERuby][], [Handlebars][], [Racket][], [Texinfo][], and [Verilog][].
Additionally, there are a new syntax checker for Javascript ([eslint][]), Ruby
(ruby-[lint][]), and YAML (js-[yaml][]).

Better Haskell support
----------------------

Despite the aforementioned removal of Hdevtools, Haskell support made a leap
forward.  The GHC syntax checker resolves local imports properly now, and has
new options to change the search path and the package databases.

The brand-new flycheck-[haskell][] extension makes use of these variables to
configure the syntax checker properly in Cabal projects.  The extensions adds
all source directories of a Cabal project to the GHC search path, and enables
the package database of the project's Cabal sandbox.

Miscellaneous new options
-------------------------

- The SASS and SCSS syntax checkers support the Compass framework now, via
  `flycheck-sass-compass` and `flycheck-scss-compass` respectively.
- Clang can enable Microsoft C/C++ extensions now, via
  `flycheck-clang-ms-extensions`.
- Rubocop can inhibit all style hints via the new `flycheck-rubocop-lint-only`.

New features
============

Syntax checker executables
--------------------------

You can now override the executables of syntax checkers.  I'll not go into
details, because this new feature is already covered by a [previous post][].

Disable syntax checkers easily
------------------------------

Flycheck as a new customization options `flycheck-disabled-checkers`, to easily
disable syntax checkers.

Previously, you needed to remove syntax checkers from `flycheck-checkers` to
disable them, either via the Customization interface, or by custom Emacs Lisp:

```commonlisp
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
```

With the new variable, there is no need for this convoluted code anymore.
Instead, just set the variable::

```
(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
```

Even better, you can easily use this variable in file or directory variables.
For instance, you can use `M-x add-file-local-variable RET
flycheck-disabled-checkers RET (emacs-lisp-checkdoc)` in your `init.el` to
disable Checkdoc warnings in your `init.el`.

Improved error list
===================

This release continues the improvements to the error list started in the last
release.  The error list is now based on Tabulated List Mode (see [#230][]), to
address a number of issues in the old Compile Mode-based error list
(e.g. misleading commands and menu items such as “Recompile”).

The new error list, which you can see in the screenshot above, fixes these
issues, and has an improved visual appearance.  The columns are aligned now, and
the superfluous file name is omitted.

Get it
======

As usual, from [MELPA][] or [Marmalade][].  I recommend the former.

[previous post]: posts/syntax-checker-executables-in-flycheck.md
[flycheck]: http://flycheck.github.io
[Source Code Pro]: https://github.com/adobe/source-code-pro
[Solarized Light]: https://github.com/bbatsov/solarized-emacs
[#230]: https://github.com/flycheck/flycheck/pull/230
[#275]: https://github.com/flycheck/flycheck/issues/275
[flycheck-hdevtools]: https://github.com/flycheck/flycheck-hdevtools
[AsciiDoc]: http://asciidoc.org/
[Cfengine]: http://cfengine.com/
[Chef]: http://www.getchef.com/
[ERuby]: http://www.kuwata-lab.com/erubis/
[Handlebars]: http://handlebarsjs.com/
[Racket]: http://racket-lang.org/
[Texinfo]: https://www.gnu.org/software/texinfo/
[Verilog]: https://en.wikipedia.org/wiki/Verilog
[eslint]: https://github.com/eslint/eslint
[ruby-lint]: https://github.com/YorickPeterse/ruby-lint
[js-yaml]: https://github.com/visionmedia/js-yaml
[flycheck-haskell]: https://github.com/flycheck/flycheck-haskell
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/packages/flycheck
[changelog]: http://flycheck.readthedocs.org/en/latest/manual/changes.html#jan-11-2014
