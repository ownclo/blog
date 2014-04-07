---
title: Talking straight about GNU's goals
tags: emacs,gnu
published: 2014-02-27
---
In a recent post to the emacs-devel mailing list, RMS talked straight about the
[goals of the GNU project][1]:

> Remember that the main purpose of the GNU system – including GNU Emacs – is
> freedom.  Technical progress and convenience are **secondary** goals.

Of course, we knew that before, but this statement is remarkably clear, and
tells us a **lot** about Emacs development: The primary goal of Emacs' umbrella
project is **not** to provide us with a fully functional editor with superior
features which support our daily work.  No, the primary goal is to defend our
freedom…

Incidentally, ironically this statement was *not* part of a discussion about
proprietary software.  It was made in a discussion about which **free** software
to use for C++ completion in Emacs:  Clang or GCC.

Clang has all the necessary features.  There are already working packages for
C++ completion based on Clang.  Nonetheless RMS strictly rejected the idea of
using Clang in Emacs, for no other reason than Clang's permissive BSD license,
which has no copyleft. Again, Clang is **free software**, even by the FSF's own
standards.  It's just **too** free for Emacs…

I don't think that this will lead us anywhere.  RMS fails to see that while his
position maintains the ideological purity of GNU, it ultimately harms its own
goals.  After all, software is just means to an end.  If a piece of software
doesn't solve our problems, users will just move on, to a different software.

So while we wait (probably forever?) for GCC to provide what Emacs needs for C++
completion, C++ developers will likely just move on, frustrated by the poor C++
support Emacs offers[^1], to free environments such as Eclipse, or to
proprietary like VisualStudio or XCode.  Whether this serves the goals of the
GNU project and the FSF, is for him to decide, but I bet that I'll disagree with
him.

[^1]: Emacs doesn't even cover all of C++ 11, four years after the initial
      release of that standard.

[1]: http://lists.gnu.org/archive/html/emacs-devel/2014-02/msg00500.html
