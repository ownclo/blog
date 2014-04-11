---
title: Search-based fontification with keywords
tags: emacs,fontification
published: 2014-03-26
---

This article continues the series about [Font Locking in Emacs][font-lock] with
a look at search-based fontification with regular expressions in
[`font-lock-keywords`][flk].  If you are new, you may want to read the first
article on [Syntactic Fontification in Emacs][syn-font-lock] first.

Search-based fontification is the main workhorse of font locking.  It is used in
any major mode to highlight the specific syntax of the target language.

The concept is quite easy, the difficulty however is in the right choice of the
regular expressions, and the matching of target syntax to the generic font lock
faces provided by Emacs.

This article illustrates the basic principles of Search-based fontification.

Prerequisites
=============

Before we can start to add font lock to our major mode, we'll have to go through
some quite essential prerequisites first:

1. Obtain a *language reference*, and read it *systematically* to extract the
   relevant language syntax.  Don't try to start with ad-hoc keywords based on
   your intuitive understanding of the language.  You'll sooner or later fail on
   corner-cases.
2. Familiarize yourself with the [`rx`][rx] macro.  We'll use it to turn
   regexp-monsters like this[^1]:

    ```common-lisp
    "\\(/\\(?:[^\n/\\]\\|\\\\.\\)*/\\)"
    ```

    into readable and commented sexps like this:

    ```common-lisp
    (rx (group
         ;; A regular expression literal is delimited by
         ;; slashes
         "/"
         (zero-or-more
          (or
           ;; Inside a regexp, a character is either
           ;; escaped with a backslash, in which case it
           ;; looses any special meaning and can't
           ;; terminate the regexp anymore,…
           (and "\\" not-newline)
           ;; …or any non-special character, namely not a
           ;; slash (this would end the sexp), not a
           ;; backslash (which would escape the subsequent
           ;; character), or a literal new-line (which is
           ;; illegal in a regexp)
           (not (any "/" "\\" "\n"))))
           "/"))
    ```

3. Familiarize yourself with [`re-builder`][reb] command.  It's a great tool to
   interactively develop regular expressions in a specific buffer.  For the
   purpose of developing font lock keywords with [`rx`][rx], change it's
   syntax, either with <kbd>C-c TAB</kbd> in re-builder, or by adding the
   following to your `init.el`:

    ```common-lisp
    (eval-after-load 're-builder '(setq reb-re-syntax 'rx))
    ```

Setup boilerplate
=================

We'll need a little boilerplate before we can start with the actual keywords.
We declare a variable to store the keywords, and tell Emacs to look for keywords
in this variable, by setting [`font-lock-defaults`][fld] in our major mode
definition:

```common-lisp
(defconst puppet-font-lock-keywords nil
  "Font lock keywords for Puppet Mode.")

(define-derived-mode puppet-mode prog-mode "Puppet" ()
  "Major mode for editing Puppet manifests.

\\{puppet-mode-map}"
  ;; …
  ;; Font locking
  (setq font-lock-defaults '((puppet-font-lock-keywords) nil nil))
  ;; …
  )
```

[`font-lock-defaults`][fld] is a list with initial settings for
fontification, with the following elements:

1. A list of variables holding keywords.  Typically this list has just a single
   element:  The name of the variable which holds our keywords[^2].
2. A boolean indicating whether to *disable*
   [syntactic fontification][syn-font-lock], and only use keywords for
   fontification.  Since syntactic fontification conveniently handles strings
   and comments in Puppet Mode, we definitely don't want to disable it, so we
   give `nil` here.
3. A boolean indicating whether our keywords are case-insensitive.  Since Puppet
   Mode has case-sensitive identifiers, we give `nil` to match the case in our
   keywords.

These are the most important settings.  There are some optional elements for
additional settings, such as a special syntax table for fontification, but these
are rarely needed, so we'll not discuss them here.  Take a look at the docstring
of [`font-lock-defaults`][fld] for more information.

Defining keywords
=================

The contents of our keyword variable `puppet-font-lock-keywords` is a list,
where each item is another list describing a single syntactic construct to
highlight.

The following is a little excerpt of the actual font lock keywords used by
Puppet Mode[^3]:

```common-lisp
(defconst puppet-font-lock-keywords
  `(
    ;; Regular expression literals
    (,(rx "/"
          (zero-or-more
           (or
            ;; Not at the end of the regexp
            (not (any "/" "\\" "\n"))
            ;; Any escaped character
            (and "\\" not-newline)))
          "/")
     0 'puppet-regular-expression-literal)

    ;; Puppet keywords
    (,(rx symbol-start
          (or "and" "case" "class" "default" "define"
              "else" "elsif" "false" "if" "in" "import"
              "inherits" "node" "or" "true" "undef"
              "unless")
          symbol-end)
     0 font-lock-keyword-face)

    ;; Class and Defined Types
    (,(puppet-rx (symbol-start
                  (or "class" "define")
                  symbol-end)
                 (one-or-more space)
                 ;; The resource name
                 (group symbol-start
                        ;; Optional top-level scope
                        (optional "::")
                        (zero-or-more
                         (any "a-z")
                         (zero-or-more (any "a-z" "0-9" "_"))
                         "::")
                         ;; Nested sub-scopes
                         (any "a-z")
                         (zero-or-more (any "a-z" "0-9" "_"))
                         symbol-end))
     1 font-lock-type-face)))
```

We specify three keywords:

1. [Regular expression literals][]
2. [Puppet keywords][]
3. [Class][] and [type][] definitions

Each keyword is a list on its own, with the following elements:

1. A regular expression to match the syntactic construct to fontify.
2. A group index, matching a number group in the regular expression.  Zero
   stands for the entire match.
3. A Lisp *expression* whose value is the face to use.

[regular expression literals]: http://docs.puppetlabs.com/puppet/3/reference/lang_datatypes.html#regular-expressions
[Puppet keywords]: http://docs.puppetlabs.com/puppet/3/reference/lang_reserved.html#reserved-words
[Class]: http://docs.puppetlabs.com/puppet/3/reference/lang_classes.html
[Type]: http://docs.puppetlabs.com/puppet/3/reference/lang_defined_types.html

Ordering
--------

Font lock keywords are processed in order of appearance.  Each keyword will
*not* match in text which is already fontified by syntactic fontification or
earlier font lock keywords[^4].  Hence **the order of keywords matters**.

For instance, with our example keywords Emacs will highlight all keywords
*first*, before classes and defined types.

This has a couple of advantages:

1. We do not need to take care for strings and comments in our font lock
   keywords.  As these are already fontified by
   [syntactic fontification][syn-font-lock], our expressions will never match
   inside strings or comments, so a keyword inside a comment will never get
   fontified.
2. We can rely on earlier font lock keywords.  In our second expression, we do
   not need to explicitly highlight `class` and `define` anymore, because
   because the earlier expression for Puppet keywords has already fontified
   these.

Consequently the order of keywords must be *carefully* constructed to avoid
matching in the wrong context.  Notably, we must specify the font lock keyword
for regular expression literals *first*, before the font lock keyword for Puppet
keywords.

Consider the following example, where the Puppet keyword `true` appears inside a
regular expression literal:

```puppet
if $foo =~ /\A(?i:true|yes)\z/ {
  notice('You speak the truth!')
}
```

In this context the keyword is not a keyword, but just literal text in the
pattern.  If the keyword for regular expression literals didn't go first, `true`
would be fontified as keyword and not as regular expression literal in this
example.

Regular expressions
-------------------

Our regular expressions for keywords and definitions are pretty straight-forward
translations from the list of [Puppet keywords][], the description of Classes_
and [Defined Types][] and the pattern for [valid resource names][] in the Puppet
language references.

The expression for regular expression literals however are an approximation of
[Regular expression literals][], which matches essentially any text between a
slash and the next unescaped slash.  This pattern is not perfect, and will match
regular expressions at inappropriate places, but as far as fontification is
considered, this is a reasonable trade-off.

We could reduce the chance of mismatched regular expressions by limiting the
keyword to match only in cases where regular expressions are actually permitted,
but this would significantly increase the complexity of our keywords and goes
beyond the scope of this article[^5].

[Puppet keywords]: http://docs.puppetlabs.com/puppet/3/reference/lang_reserved.html#reserved-words
[Classes]: http://docs.puppetlabs.com/puppet/3/reference/lang_classes.html
[Defined Types]: http://docs.puppetlabs.com/puppet/3/reference/lang_defined_types.html
[Valid resource names]: http://docs.puppetlabs.com/puppet/3/reference/lang_reserved.html#classes-and-types
[regular expression literals]: http://docs.puppetlabs.com/puppet/3/reference/lang_datatypes.html#regular-expressions

### Symbol boundaries

We use the special forms `symbol-start` and `symbol-end` to make sure that our
expressions only match entire symbols.  These special forms match the empty
string at the beginning and end of a symbol.

This prevents our expressions from matching keywords which appear in the middle
of another identifier, e.g. in a function name.  In the following example code,
our patterns will match the `define` keyword in the type definition, but not
“define” in call of the `defined` function [^6]:

```puppet
define foo($bar = $title) {
  if !defined(Package[$bar]) {
    package { $bar: ensure => installed }
  }
}
```

<div class="alert alert-info">

**Note!**

Don't be confused by the syntax highlighting on this page.  It's provided by a
separate tool named [Pygments][], which has different rules for highlighting
Puppet Code.  That's why you see `defined` being highlighted here.

</div>

[Pygments]: http://pygments.org/

### Grouping

Our first two expressions for regexp literals and keywords do not have match
groups, since in both cases we simply want to highlight the entire construct.
Hence we give the match index `0` to highlight the entire match of the regular
expression.

In the third expression for class and type definitions however we only want to
highlight the name of the new class or type, but not the preceding keyword,
which was already handled by our previous expression for Puppet keywords.
Hence, we put the pattern for the class and type name into a match group, and
give its index `1` to only apply the highlighting to the text matched by this
group.

Faces
-----

The face is arguably the most important part in a font lock keyword.  It's what
the user will ultimately see when using your mode.  Thus, the choice of good
faces is crucial for good fontification.

### Standard faces

Luckily, Emacs provides a good set of [standard font lock faces][] for various
common syntax elements.  We use two of these faces in our keywords:

[`font-lock-keyword-face`][flkf]
  ~ A face for the keywords of programming languages

[`font-lock-type-face`][fltf]
  ~ A face for names of types and classes

Font lock provides a lot more faces for comments, strings, constants, variable
names, function names, builtins, preprocessor instructions, and many more.

You should use the standard faces whenever possible.  It's not only easier for
you to just pick an appropriate standard face, it's also good for your users if
your mode is a good Emacs citizen in this regard:

- Fontification will look the same across different modes.  A string, a comment
  or a keyword always look the same, regardless of whether they are in C++ code,
  Python code, or Emacs Lisp code.
- Color themes will automatically affect your mode.  Color themes just need to
  set the standard faces to style fontification in any programming language
  Emacs has a mode for, and users can install new color themes without having to
  care for whether a theme supports your specific mode or not.

[standard font lock faces]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html#Faces-for-Font-Lock

### Custom faces

Sometimes the standard faces are just not appropriate, however.

In our example, we want to fontify regular expression literals, but there's no
`font-lock-regexp-face`, and none of the existing standard faces are really
appropriate.  The best fit are still [`font-lock-string-face`][flsf] or
[`font-lock-constant-face`][flcsf], but a user may probably want to distinguish
regular expressions from strings or constants.

For this purpose, we define a custom face with the name
`puppet-regular-expression-literal`, using [`defface`][defface]:

```common-lisp
(defface puppet-regular-expression-literal
  '((t :inherit font-lock-constant-face))
  "Face for regular expression literals in Puppet."
  :group 'puppet)
```

The second argument to [`defface`][defface] takes our face definition.  The
syntax of face definitions is fairly intricate, and beyond the scope of this
article.  The docstring of [`defface`][defface] is a good start for more
information.

Our definition is very simple, however: The new face just inherits all its
attributes from the standard [`font-lock-constant-face`][flcsf], so by default a
regular expression will look like a constant.

By inheriting from a standard font lock face, we play nicely with color themes
which only support the standard font lock faces, and we do not need to come up
with a good default style for our new face.  For these reasons, you should try
hard to find a built-in Emacs face to inherit from whenever you need to define
custom faces.

Nonetheless, users can customize our face *independently* from
[`font-lock-constant-face`][flcsf] to make regular expressions appear distinct
from constants if they desire, since it is a completely separate face.

The subsequent arguments in the face definition are a docstring, and standard
[`defcustom`][defcustom] arguments.  We just give `:group`, to make our custom
face appear in <kbd>M-x customize-group RET puppet</kbd>, where all the options
of Puppet Mode reside.

You'll notice that unlike the standard faces our custom face doesn't have the
suffix `-face`.  The Emacs Lisp reference recommends against this in
[Defining Faces][].

The standard faces however predate this convention, and keep their names for
backwards compatibility.  With regards to faces, you should *not* follow Font
Lock as a template.

[Defining Faces]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html

### Face variables versus faces

When looking closely at our font lock keywords, you'll notice that we specify
standard font lock face *without* quoting, whereas our custom face is quoted.

The reason is that font lock additionally defines *variables* for its faces.  A
comment in `font-lock.el` explains the background, and discourages this practice
for custom faces[^7]:

> Originally these variable values were face names such as `bold` etc.  Now we
> create our own faces, but we keep these variables for compatibility and they
> give users another mechanism for changing face appearance.  We now allow a
> *facename* in [`font-lock-keywords`][flk] to be any expression that returns a
> face.  So the easiest thing is to continue using these variables, rather than
> sometimes evalling *facename* and sometimes not.  sm.

> Note that in new code, in the vast majority of cases there is no need to
> create variables that specify face names.  Simply using faces directly is
> enough.  Font-lock is not a template to be followed in this area.

Remember that the face part of a font lock keyword is not a face name, but
rather an *expression returning a face name*.  Hence, we can use the standard
face without quoting:  They are expressions, which evaluate to the value of the
variable, which in turn gives the face name…which—to make matters even
worse—has usually the same name as the variable.

For our custom faces however we do not define extra variables.  Hence we must
quote the face name, so that font lock uses it literally instead of trying to
evaluate it as variable.

While developing font lock keywords, you need to be aware of this small, but
important difference, lest you introduce subtle bugs into your code.

Limits
======

We have seen that font lock keywords provide a simple, yet powerful facility to
add syntax highlighting for complex syntactic constructs.

There is a limitation to their power, however: They do not take the syntactic
context into account.  It is thus impossible to define a keyword which applies
only inside a comment, or only inside single-quoted strings.  For instance, font
lock keywords alone cannot highlight variable expansions inside strings, as in
Ruby or Puppet.

To handle such special syntax, which depends on the surrounding syntactic
context, you need to hook into the syntactic analyzer of Emacs, and explicitly
identify special constructs, a technique, which I'll cover in a later post of
this series.

Conclusion
==========

Font lock keywords are *the* main part of fontification in Emacs.  Any mode uses
them to highlight the syntax of the corresponding language, and no mode can go
without them.  Good font lock keywords are crucial for good syntax highlighting
in Emacs.

However, for really stellar and awesome highlighting, which even goes as far as
fontifying variables or expressions inside strings, a major mode needs to go
beyond them, and use more intricate techniques of syntactic analysis.  More on
that in a later article.

So long

[^1]: This is the expression for regular expression literals in Puppet.

[^2]: Further elements may be used to give additional keywords for multiple
      levels of fontification, a feature which comes from days where fully
      featured fontification was too slow for most computers.  Higher levels
      contained more complex keywords, and a user would typically customize
      Emacs to enable the highest possible level their computers could handle.

      Nowadays however even the weakest computers easily cope with fully
      featured fontification, so most major modes just give a single level of
      fontification.  Most modern Emacs users don't even know about this
      feature anymore.

[^3]: Puppet Mode has a lot more font lock keywords of course.  However, these
      two are sufficient to illustrate the basic principles of font lock
      keywords.

[^4]: Technically this is not completely true.  Keywords can be specified to
      override earlier fontification.  However, this should be used with care.
      Notably, it will cause font lock keywords to be applied within comments
      and strings as well, which is not generally desirable.

[^5]: Puppet Mode will actually use a more intricate handling of regular
      expression literals, which takes the syntactic context into account, and
      also improves the behaviour of various navigation commands.  See
      [issue #39][#39] for details.

[^6]: In case you are wondering that `defined` is nonetheless highlighted in a
      Puppet buffer: In the real Puppet Mode, we have an additional font lock
      keyword to highlight all builtin functions of Puppet with
      [`font-lock-builtin-face`][flbf].

[^7]: You can see the original comment in your Emacs by navigating to the
      definition of the variable [`font-lock-comment-face`][flcf] with <kbd>M-x
      find-variable RET font-lock-comment-face</kbd>, or in your browser in
      Emacs' Git web interface, at [font-lock.el, line 292][l292].

[font-lock]: internal:posts/font-locking-in-emacs.md
[syn-font-lock]: internal:posts/syntactic-fontification-in-emacs.md
[reb]: el-function:re-builder
[flk]: el-variable:font-lock-keywords
[fld]: el-variable:font-lock-defaults
[rx]: el-function:rx
[flkf]: el-variable:font-lock-keyword-face
[flbf]: el-variable:font-lock-builtin-face
[flcf]: el-variable:font-lock-comment-face
[flcsf]: el-variable:font-lock-constant-face
[fltf]: el-variable:font-lock-type-face
[flsf]: el-variable:font-lock-string-face
[defface]: el-function:defface
[defcustom]: el-function:defcustom
[l292]: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el?h=trunk&id=b8392964d2735a0ac3230ddfbbfab8a82d02415d#n292
[#39]: https://github.com/lunaryorn/puppet-mode/pull/39
