---
title: Syntactic fontification in Emacs
published: 2014-03-12
tags: emacs,fontification
---

This is the first article of the [Font Locking in Emacs][font-lock] series.  It
explains the concept of “syntactic fontification” of strings and comments.  This
font locking technique is the easiest to get started with, although the concept
itself is quite intricate and powerful.

Syntax tables
=============

Every major mode defines a so-called [Syntax Table][].  A syntax table defines
the syntactic role of a *single character* with various [Syntax Descriptors][].
Emacs uses this information for navigation commands and font locking.

When you move over a symbol with [`forward-symbol`][fs], it's the syntax table
that tells Emacs where the symbol ends.  It's also the reason, why
[`forward-word`][fw] stops a dash in Emacs Lisp Mode, whereas
[`forward-symbol`][fs] moves over a dash and stops at the next whitespace or
parenthesis: The syntax table of Emacs Lisp mode categorizes the dash as a
symbol character, so commands working on words ignore it.

The symbol table also drives the fontification (aka “highlighting”) of strings
and comments.  Whenever Emacs sees a character which is categorized as string or
comment delimiter, subsequent text is fontified as string and comment
respectively until a matching delimiter is reached.

The real power of this feature is hidden beneath the phrase “matching
delimiter”: If the syntax table is setup properly, Emacs automatically skips
across escaped string delimiters inside strings, or across nested comments!

<div class="alert alert-info">

**Note!**

In this article we'll only cover the Syntax Table as far as
fontification is concerned.  Generally, Syntax Tables of major modes are much
larger, and also specify word and symbol constituents, paired delimiters, and
more, which is important for word, symbol and sexp navigation commands (i.e. the
standard `forward-*` and `backward-*` family of commands).

When writing the Syntax Table for your own major mode, be sure to consider this
as well.  At best, read the entire [Syntax Table][] section in the Emacs Lisp
reference.

</div>

Strings
=======

Puppet has two types of strings enclosed in single and double quotes
respectively.  Let's define a syntax table that understands these strings:

```common-lisp
(defvar puppet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    table)
  "Syntax table in use in `puppet-mode' buffers.")
```
  
[`make-syntax-table`][mst] creates a standard syntax table, to which we add our
custom classifications with [`modify-syntax-entry`][mse].  This function takes
three arguments:

1. The character to classify
2. The new syntax descriptor for the character
3. The table to modify

[Syntax Descriptors][] are fairly intricate, but the basic structure is quite
simple.  A descriptor is a string, where each character has a special meaning.
In our example, the descriptors are simple two-character strings.

The first character in this string defines the **syntax class** (see [Syntax
Class Table][]).  The quotation mark `"` denotes the “String quotes” class,
i.e. characters which delimit strings.

The second character denotes the *matching character*.  Unsurprisingly, for our
strings the matching character is the same as the character we classify.  By
specifying the right matching character we make sure that Emacs does not
consider a single-quoted to be ended by a double-quote.

Any further characters are [Syntax Flags][], which we currently don't need.

This little table is (almost) everything that's needed for proper fontification
of strings!  We don't even need to explicitly enable this table in our major
mode, because [`define-derived-mode`][ddm] (the standard way to declare major
modes) already does that for us!

Escape characters
=================

We've just missed a little detail: Emacs doesn't yet know about the escaping
character.  As such, it cannot yet detect an escaped string delimiter inside a
string.  This is easily fixed, however.  Just like any other language, Puppet
uses the backslash to escape string delimiters, we just need to tell Emacs about
this with one extra entry in our syntax table:

```common-lisp
(modify-syntax-entry ?\\ "\\" table)
```

This entry puts the backslash into the “Escape characters” class (fittingly
denoted by the backslash).  Now we are really done: With **just three entries**
in our syntax table, Emacs correctly fontifies Puppet's string syntax!

Comments
========

Syntactic fontification also handles comments.  Puppet has two types of
comments:

- Shell style comments, starting with a dash `#` and ending with a new line
- C-style comments enclosed in `/*` and `*/`

The first kind is easy:

```common-lisp
(modify-syntax-entry ?# "<" table)
(modify-syntax-entry ?\n ">" table)
```

The first entry puts `#` into the “Comment Starters” class, i.e. the class of
characters which start comments, and the second one the line break into the
class of “Comment enders”, i.e. characters which terminate comments.

Now we have line comments, but what about C style comments?  These seem to be a
little more intricate, since their terminators are two-character sequences.  And
to make matters even more complicated, `/` and `*` have an entirely
different meaning when appearing as a single character: They are operators for
division and multiplication respectively!

Luckily, Emacs has this common case covered already.  Remember the [Syntax
Flags][] briefly mentioned in the beginning?

Syntax flags provide additional information about characters, and some of these
flags are concerned with two-character syntax.  From [Syntax Flags][]:

- “`1` means C is the start of a two-character comment-start sequence.
- `2` means C is the second character of such a sequence.
- `3` means C is the start of a two-character comment-end sequence.
- `4` means C is the second character of such a sequence.”

With these flags, we can now tune the descriptors of the `/` and `*`
characters to reflect their comment syntax, as well as their operator meaning::

```common-lisp
(modify-syntax-entry ?/ ". 14" table)
(modify-syntax-entry ?* ". 23" table)
```

Both characters are put into the “Punctuation characters” class, which is
commonly used for operators.  The slot for the matching character is blank,
since as operators are naturally no paired characters.

The subsequent flags now specify their comment syntax:

- With `14` we tell Emacs that `/` is the first character of a
  comment-start sequences and the second character of a comment-end sequences,
- and with `23` we denote that `*` is goes second in the comment-start and first
  in the comment-end sequences.

This nicely specifies our comment syntax, where `/*` starts a comment which is
then ended by `*/`.  The order of flags doesn't matter.

When you try this, you'll notice a serious flaw, though:  A comment started
`/*` ends at the next line break!  Apparently Emacs doesn't yet understand,
that these are actually two *different* styles of comments.

We need make Emacs aware of this difference with another flag.  From [Syntax
Flags][]:

- `b` means that C as a comment delimiter belongs to the alternative "b"
  comment style.  For a two-character comment starter, this flag is only
  significant on the second char, and for a 2-character comment ender it is only
  significant on the first char.”

By adding this flag, we tell Emacs to consider both comment styles
independently, so we replace our first attempt with the following lines:

```common-lisp
(modify-syntax-entry ?/ ". 14b" table)
(modify-syntax-entry ?* ". 23b" table)
```

Now we have two independent comment styles for Puppet files.

Limits
======

Syntax tables are a powerful tool to fontify strings and comments.  We have seen
that we can declare different, independent styles of strings and comments, and
handle escaping in strings as well as more intricate comment syntax.  As such,
syntax table can easily cope with comments and strings in many programming
languages.

But as always, there are some limitations.  With the tiny exception of
two-character comment sequences, syntax tables are **stateless**.  They only
look at the character itself, and do not consider its position in a file or
block or its adjacent characters.

As such, whenever the syntactic properties of a character depend on adjacent
characters, or on a specific position on a line, syntax tables are out of scope.
In such cases, you need to implement fontification in a different way.  Let's
briefly look at two common cases.

In many configuration file types the dash starts a comment only when at the
beginning of a line or preceded by whitespace only.  A common example is
`.gitignore`.  Since the syntax of such configuration files is very simple
normally, it's usually sufficient to simply disable syntactic fontification with
an appropriate entry in [`font-lock-defaults`][fld], and fontify comments with
[Search-based fontification][], which I'll cover in the next article of this
series.

A more intricate example for the limits of Syntax Tables are strings delimited
by multiple characters, such as triple-quoted strings in Python.  In such cases
its not sufficient to just use Search-based fontification:

- Such strings usually span many lines, which breaks the line-oriented search
  pattern of Search-based fontification.
- It's usually important to properly classify the syntax of such strings.
  Without such a classification, navigation commands such as
  [`forward-sexp`][fsexp] will not be able to recognize and skip strings
  correctly.  Also, many 3rd party extensions such as the popular
  [Smartparens][] rely on syntactic information.  For instance, Smartparens uses
  syntactic information to make pairs available in specific contexts only.

To deal with these issues, Python Mode hooks into the syntactic analysis of
Emacs, identifies triple-quoted strings explicitly, and adds special text
properties to mark their contents as strings.  This is quite a complicated
approach, which I'll probably also cover in a later article in this series.

Conclusion
==========

We've seen that Syntactic fontification is a simple, yet powerful approach to
fontify the generic syntax of strings and comments, which handle the common
variants of string and comments syntax well, but have a very limited ability to
deal with stateful syntax.

In the next article of this series, we'll look at fontifying specific syntactic
elements of the Puppet language with [Search-based fontification][].

Stay tuned!

[font-lock]: internal:posts/font-locking-in-emacs.md
[fs]: el-function:forward-symbol
[fw]: el-function:forward-word
[fsexp]: el-function:forward-sexp
[fld]: el-variable:font-lock-defaults
[mst]: el-function:make-syntax-table
[mse]: el-function:modify-syntax-entry
[ddm]: el-function:define-derived-mode
[Syntax Table]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
[Syntax Descriptors]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Descriptors.html
[Syntax Class Table]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
[Syntax Flags]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html 
[Search-based fontification]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
[Smartparens]: https://github.com/Fuco1/smartparens
