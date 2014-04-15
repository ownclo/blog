---
title: Colophon
---

- Written with [Emacs][], because I can't get the hang of Vim.  Take a look at
  my [Emacs configuration][].
- Written (mostly) on OS X, because Linux is such a poor desktop OS.
- Highlighted with [Pygments][], because source code should be pretty.  I use
  some [custom glue code][] to use Pygments with Pandoc.
- Rendered with [Pandoc][], because it's the
  [best Markdown processor][pandoc-md] around.
- Generated with [Hakyll][], because is a powerful tool, and because it's cool
  to use Haskell for a blog.
- Styled with [Bootstrap][], because I'm a
  [programmer and computer scientist][about] and no web designer.
- Build on [Travis CI][travis], because it's super-convenient to have my blog
  built automatically after every commit.  Take a look at the
  [Travis configuration][] and the [deployment script][].
- Hosted on [Github Pages][], because I don't have any other place for it.

[travis]: https://travis-ci.org/lunaryorn/blog
[Emacs]: http://www.gnu.org/software/emacs/
[Emacs configuration]: https://github.com/lunaryorn/stante-pede
[Pygments]: http://pygments.org/
[custom glue code]: https://github.com/lunaryorn/blog/blob/master/src/Text/Highlighting/Pygments/Pandoc.hs
[Pandoc]: https://github.com/jgm/pandoc "Pandoc"
[pandoc-md]: http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown
[Hakyll]: https://github.com/jaspervdj/hakyll
[Bootstrap]: http://getbootstrap.com/
[about]: internal:pages/about.md
[Travis configuration]: https://github.com/lunaryorn/blog/blob/master/.travis.yml
[deployment script]: https://github.com/lunaryorn/blog/blob/master/travis-deploy.bash
[Github Pages]: https://github.com/lunaryorn/lunaryorn.github.io
