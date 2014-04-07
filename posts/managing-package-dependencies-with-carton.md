---
title: Managing Package Dependencies with Carton
tags: emacs,carton,cask
published: 2013-04-10
---

<div class="alert alert-warning">

**Update!** <small>Mar 28, 2014</small>

Carton was renamed to Cask some time ago.  All links in this article were
updated accordingly

While a lot of the information in this article is still correct, you need to
read it with care.

</div>

[Carton][] is a little utility to manage Emacs packages.  Simply speaking, it is to
`package.el`, what Bundler is to Rubygems.

You can use it to manage the packages in your Emacs configuration, or the
development dependencies of packages that you are developing.

Carton works around a `Carton` file, in which package repositories and packages
are specified.  The command `carton` works on this file, and provides commands
to install and update the packages in a `Carton` file.

Getting started
===============

Just install Carton following the [instructions on the project page][install].

Carton in `~/.emacs.d`
======================

In your personal Emacs configuration Carton basically provides a central place
to specify the packages you want to use in your configuration.  With Carton, you
do not longer need to add your packages directory to Git or clutter your
configuration files with `(package-install)` calls.

To use Carton in your personal Emacs configuration, create a file
`~/.emacs.d/Carton`.  A simple `Carton` file looks like this:

```commonlisp
(source "melpa" "http://melpa.milkbox.net/packages/")

(depends-on "solarized-theme")
(depends-on "magit")
(depends-on "flycheck")
```

The syntax is rather simple:

- A `source` line adds a 3rd party package repository, in this example the
  compulsory [MELPA][] repository.
- A `depends-on` line specifies a package to install, in this case, in this case
  the fancy [Solarized][] theme, the awesome Git frontend [Magit][] and my syntax
  checking library [Flycheck][].

Read the Carton documentation for more details on the syntax.

A real-world `Carton` file is just as simple as this example, as you can see in
the `Carton` file from [my Emacs configuration][my-cask-file].

Instead of <kbd>M-x package-install</kbd> and <kbd>M-x list-packages</kbd> to
install and update packages, you now use the `carton` command:

```console
$ cd ~/.emacs.d/
# Installing all packages from Carton
$ carton install
# Updating all packages from Carton
$ carton update
```

<div class="alert alert-warning">

**Warning!**  You need to re-start Emacs after each of these commands.

</div>

To install a new package, simply add it to the `Carton` file:

```console
$ cd ~/.emacs.d/
$ echo '(depends-on "company")' >> Carton
$ carton install
```

After restarting Emacs, the [Company][] package will now be available in Emacs.

Carton for package development
==============================

Carton can also manage the runtime *and development* dependencies of the Emacs
packages that you develop.  As in your configuration it provides a central place
to specify dependencies, and also install them *locally* in the source
directory.  It keeps your Emacs configuration clean of packages for development,
and your source directory clean of submodules.

Carton also handles the `load-path` for you, and allows you to execut e commands
with a `load-path` that includes all locally installed packages.

Again you start with a `Carton` file, like the one of [Flycheck][] itself:

```commonlisp
(source "melpa" "http://melpa.milkbox.net/packages/")

(package-file "flycheck.el")

;; Various modes for use in the unit tests
(development
 (depends-on "coffee-mode")
 (depends-on "haml-mode")
 (depends-on "js2-mode")
 (depends-on "js3-mode")
 (depends-on "lua-mode")
 (depends-on "cperl-mode")
 (depends-on "php-mode")
 (depends-on "php+-mode")
 (depends-on "sass-mode")
 (depends-on "scss-mode")
 (depends-on "go-mode")
 (depends-on "rust-mode"))
```

This file uses two other directives of Carton, specifically targeted at package
development:

- `package-file` declares the “main” file of your package.  Carton extracts the
  package meta data (name, version and description) and the runtime dependencies
  from the [standard package headers][headers] of this file.  Alternatively you
  can use `package` and `depends-on` to declare the meta data and the runtime
  dependencies directly .
- `development` scopes contained dependencies to development only.

Now you can install all these dependencies *locally* into your source directory,
and run arbitrary shell commands with a `load-path` that includes these
packages:

```console
$ carton install
$ carton exec make test
```

The first command installs all packages from the `Carton` file into an `elpa`
directory alongside the `Carton` file.  `carton exec` then runs the given
command (`make test` in this case) with a `load-path` that includes all of these
packages.

You can also embed Carton directly into your test runner script, e.g. as
`tests/run.sh`:

```bash
#!/bin/sh

if [ -z "$EMACS" ]; then
  export EMACS=emacs
fi

carton exec "${EMACS}" -Q --no-site-lisp --script \
  "$(dirname $0)/flycheck-testrunner.el" "$@"
```

Now `tests/run.sh` executes all Flycheck tests with correct dependencies.

Conclusion
==========

Carton provides an easy and convenient way to manage all of your Emacs packages,
keeping your Emacs configuration and your packages clean of all the package
management hassle.

The utility is actively maintained and covered by a comprehensive test suite,
and it's maintainer responds very fast to issues and pull requests.

In short, the sooner Carton becomes part of your tool chain, the better!

[my-cask-file]: https://github.com/lunaryorn/stante-pede/blob/master/Cask
[carton]: https://github.com/cask/cask
[install]: http://cask.github.io/installation.html
[melpa]: http://melpa.milkbox.net
[solarized]: https://github.com/bbatsov/solarized-emacs
[magit]: https://github.com/magit/magit
[flycheck]: https://github.com/flycheck/flycheck
[doc]: https://github.com/rejeep/carton/blob/master/README.md
[company]: http://company-mode.github.io/
[headers]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html#Library-Headers
