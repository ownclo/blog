---
title: Unbreaking Marmalade
published: 2014-03-28
tags: emacs
---

<div class="alert alert-info">

**Update!** <small class="text-right">Apr 1, 2014</small>

It's on [Hackage][] now.  The installation instructions below were updated
accordingly.

</div>

<div class="panel panel-default text-center">
<div class="panel-body">
<img src="/images/marmalade-crash.png" class="img-responsive img-thumbnail">
</div>
<div class="panel-footer">
The dreaded Marmalade Screen of Death
</div>
</div>

Looks familiar?  Then you're probably a fellow Emacs developer trying to upload
a package to the [Marmalade][] ELPA archive.

We've been getting this fancy error for some time now, and there's already a
nice [issue][] for it, but no little progress.  Just like, er, well… in the
entire Marmalade itself.  Sure, there is Nic Ferrier's brand-new Emacs Lisp
Marmalade rewrite super-thing, but… ok, even Duke Nukem made it to a release
eventually, so we have some hope left, haven't we?

And while we're excitedly waiting for Marmalade being reborn like a pheonix from
the ashes, I'd like to share [marmalade-upload][] with you.  It's a little
Haskell tool to upload packages to Marmalade via its API, which isn't broken yet
fortunately (don't hold your breath, folks).

To get started, install [Haskell Platform][] and then type:

```console
$ cabal install marmalade-upload
```

It'll take a while to fetch and build all the deps, but when it's done, upload
your new packages with `marmalade-upload USERNAME PACKAGE-FILE`, e.g.:

```console
$ marmalade-upload lunaryorn dist/flycheck-0.18.tar
```

The tool will ask for your Marmalade password, and upload the package.  For
extra safety, it checks the mimetype of the given file first, and throws an
error if it's invalid.  If you're on OS X or KDE, the tool will put your
Marmalade login token (not your Marmalade password!) into the keychain, so you
won't be prompted for your password next time.

And, yes, sorry for the inconvenient language, but it's a boring tool for a
stupid bug, so I at least try to learn a bit from it by refreshing what little
of my Haskell skills is still there.

In a related development, [MELPA][] recently [announced][] that they are
building [stable packages][] now from DVCS (currently Git only) tags, which is a
*big* step closer to Marmalade's final passing.  So long, dear Marmalade, we had
hard time with you, and we're glad that's over soon.

[Hackage]: http://hackage.haskell.org/package/marmalade-upload
[Marmalade]: http://marmalade-repo.org/
[issue]: https://github.com/nicferrier/marmalade/issues/73
[marmalade-upload]: https://github.com/lunaryorn/marmalade-upload
[Haskell Platform]: http://www.haskell.org/platform/
[MELPA]: http://melpa.milkbox.net/
[announced]: http://www.reddit.com/r/emacs/comments/216jhc/stable_packages_from_melpa/
[stable packages]: https://github.com/milkypostman/melpa#stable-packages
