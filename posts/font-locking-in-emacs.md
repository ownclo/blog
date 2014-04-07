---
title: Font Locking in Emacs
tags: emacs,fontification
published: 2014-03-12
---

Font locking (aka syntax highlighting) is an essential part of any Emacs major
mode.  While Emacs has great facilities to implement syntax highlighting, these
are not always easy to understand and implement.

This post is the start of an article series about font locking that aims to give
a little overview about various font locking techniques in Emacs.  In essence,
it's a summary of what I discovered and learned recently while refactoring and
improving font locking in [Puppet Mode][].

As such, the examples throughout this series are about Puppet Mode.  If you are
not familiar with Puppet, you may want take a short look at the
[Puppet language reference][] first.

Overview
========

Emacs has two major facilities for syntax highlighting:

Syntax Tables

:   Simple generic and stateless highlighting for strings and comments by
    classifying individual characters.

    Most major modes make use of this basic facility for strings and comments.

Font Lock Keywords

:   Stateless highlighting based on regular expressions, for specific syntactic
    elements of the target language.

    This is the major workhorse of font lock, used by every major mode.

Both facilities are *stateless*, and cannot look at syntactic context.  However,
by means of [`syntax-propertize-function`][spf] Emacs allows major modes to
hook into its syntactic analysis, to add arbitrary text properties and syntax
classifiers to buffer text.

This is the most powerful, but also the most tricky syntax highlighting feature
of Emacs.  As such, it's not frequently used, but sometimes it's necessary[^1],
and often it makes the difference between good and great syntax
highlighting[^2].

Articles
========

In this series, I'll cover all of these three ways of font locking:

1. [Syntactic Fontification in Emacs](internal:posts/syntactic-fontification-in-emacs.md)
2. [Search Based Fontification with Keywords](internal:posts/search-based-fontification-with-keywords.md)
3. Advanced syntactic fontification (not yet written)

I'll update this post as new articles in this series appear.

Enjoy this series, and feel free to ask me any questions or give me feedback.
At best, ping me on [Twitter][] or [open an issue][].

[^1]: Python Mode needs to use this feature to fontify triple-quoted strings.

[^2]: Ruby mode uses this feature to fontify variable expansions inside
      double-quoted strings.

[spf]: el-variable:syntax-propertize-function
[Puppet Mode]: https://github.com/lunaryorn/puppet-mode
[Puppet language reference]: http://docs.puppetlabs.com/puppet/latest/reference/lang_visual_index.html
[Twitter]: https://twitter.com/lunaryorn
[open an issue]: https://github.com/lunaryorn/blog/issues
