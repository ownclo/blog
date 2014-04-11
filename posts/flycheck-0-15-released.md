---
title: Flycheck 0.15 released
published: 2013-11-05
tags: emacs,flycheck
---

I have the honour and pleasure to announce a new release of [Flycheck][], the
modern syntax-checking extension for Emacs:

<div class="panel panel-default text-center">
<div class="panel-body">
<img src="/images/flycheck-0.15.png" class="img-responsive img-thumbnail">
</div>
<div class="panel-footer">
Flycheck 0.15 with [Zenburn][] and [Source Code Pro][]
</div>
</div>

It is three months since the last release, so this release brings quite a lot of
changes.  For a complete list of all changes, please read the complete list of
[changes][].  In this post, I will just go through the most important ones.

Breaking changes
================

This release introduces three breaking changes.  Two of these are related to the
[new error list][]:

<div class="alert alert-warning">

**Warning!**

`flycheck-display-errors-in-list` is gone.  If you had previously set
`flycheck-display-errors-function` to this function, **remove** this setting
from your configuration.

</div>

Besides, `flycheck-list-errors` does not take a prefix argument anymore.  It
cannot list errors at point any longer.

The third breaking change is the removal of `flycheck-declare-checker`, which
was obsolete already since the last release.  Unless you have custom syntax
checkers not yet ported to `flycheck-define-checker`, this won't affect you in
any way.  I am not aware of any 3rd party extension which still uses
`flycheck-declare-checker`.

New syntax checkers
===================

Two new languages made it into this release:  YAML (using the YAML parser from
the Ruby standard library) and [Slim][].

Additionally there are new syntax checkers for Javascript and PHP.  Flycheck can
check Javascript with Google's [Closure Linter][], in place of Jshint.  For PHP,
Flycheck now uses the [PHP Mess Detector][] to check for semantic errors, in
*addition* to a syntax checks with PHP CLI and a style checks with PHP
CodeSniffer.

New error list
==============

The error list at :kbd:`C-c ! l` has been redesigned.

It is no longer a static list filled once by :kbd:`C-c ! l`.  Instead, it
automatically updates after each syntax check, and follows the current window,
i.e. if you switch to another window, the error list is updated to show the
errors of the corresponding buffer.

Furthermore, the error list highlights the errors at point and at the current
line.  If you move the point to an error location, the error list automatically
scrolls to the corresponding error, and highlights it with the new
`flycheck-error-list-highlight-at-point` face.  Additionally, it highlights all
other errors at the current line with the new `flycheck-error-list-highlight`
face.

C/C++ support
=============

The `c/c++-clang` syntax checker for C and C++ got a bunch of new options:

- Set additional preprocessor definitions for syntax checking with
  `flycheck-clang-definitions`, corresponding to the `-D` option for `clang`.
- Include additional headers or files during syntax checking with
  `flycheck-clang-includes`, corresponding to the `-include` option for `clang`.
- Choose the language standard, e.g. C++98 or C++11, with
  `flycheck-clang-language-standard`, corresponding to the `-std` option for
  `clang`.
- Disable RTTI during syntax checking with `flycheck-clang-no-rtti`,
  corresponding to the `-fno-rtti` option for `clang`.  In current Clang
  versions, however, this does not cause errors or warnings when using RTTI.
- Choose the standard library for syntax checking with
  `flycheck-clang-standard-library`, corresponding to the `-stdlib` option for
  `clang`.  Currently, Clang supports `libstdc++` for the good old GNU standard
  library, and `libc++` for the modern Libc++ from the LLVM project.

Besides, the Clang syntax checker was changed to correctly handle local include
files, e.g. `#include "foo.h"`.

New `info` level messages
=========================

In addition to the `warning` and `error` levels, Flycheck now has a new `info`
level, which is intended for informational messages which provide additional
information about a specific location in the source code.  With this new level,
the semantics of error levels is as follows:

.. class:: dl-horizontal

`error`
  Definite errors which must be fixed for the source code to work correctly
`warning`
  Potential errors and issues, which can be ignored, but still deserve to be
  fixed
`info`
  Additional information about a specific source code location, which does not
  indicate an error or issue, but is still worth noting

Some syntax checkers were changed to use this new level for messages, which do
not really fit into the `warning` level:

- `c/c++-clang` for `note:` messages
- `python-flake8` for PEP8 naming issues emitted by the `pep8-naming` plugin
- `python-pylint` for convention level messages, e.g. naming issues, etc.

The new level is already supported by the popular [Solarized][] and [Zenburn][]
themes.

Custom error levels
===================

The new `info` level is backed by a generic mechanism to define new error
levels, using the new function `flycheck-define-error-level`.  For instance, the
`warning` level is now defined as follows:

```common-lisp
(flycheck-define-error-level 'warning
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'question-mark
  :fringe-face 'flycheck-fringe-warning)
```

A error level consists of a category for overlays, which defines the appearance
and priority of overlays for this level, and bitmap and face for fringe
indicators.

After defining an error level, you can use it the error patterns of a syntax
checker as usual.

Other improvements
==================

Beside these important changes, there are also a number of smaller improvements:

- Flycheck does not longer check encrypted files for obvious reasons.
- The `emacs-lisp-checkdoc` syntax checker does not check `.dir-locals.el`
  anymore.
- `python-pylint` now parses error columns from the output of `pylint`.
- Spurious “flawed definition” warnings in `lua`, `rst` and `go-build` were
  fixed.
- `c/c++-cppcheck` output now parses correctly when using the pure Emacs Lisp
  XML parser in `xml.el`.

    <div class="alert alert-info">

    **Note!**

     Nonetheless, you are advised to use Emacs with `libxml` support.  Most
     Linux distributions ship Emacs packages with `libxml` support, but if you
     are building your own, or use a source-based distribution such as Gentoo,
     take care to enable `libxml` for Emacs.

    </div>

Get it
======

As usual, from [MELPA][] or [Marmalade][].  I recommend the former.

[flycheck]: http://flycheck.github.io
[Source Code Pro]: https://github.com/adobe/source-code-pro
[changes]: http://flycheck.readthedocs.org/en/latest/manual/changes.html#nov-15-2013
[Slim]: http://slim-lang.com
[Closure Linter]: https://code.google.com/p/closure-linter/
[PHP Mess Detector]: http://phpmd.org/
[Solarized]: https://github.com/bbatsov/solarized-emacs
[Zenburn]: https://github.com/bbatsov/zenburn-emacs
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/packages/flycheck
