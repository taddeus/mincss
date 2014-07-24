mincss is an extendible CSS minifier written in OCaml. It contains a complete
parser for the CSS3 language, along with consistent type definitions and a
traversal utility function for use in transformation passes.

mincss is currently still in development, finished components are the parser,
stringification (along with whitespace compression), and color compression.
Rulset merging is partially documented below but currently unimplemented.


Features
========

- Whitespace compression
- Color compression
- Creation of shorthand properties (e.g. `font` and `background`)
- Ruleset merging (see below)
- Command-line interface and web interface


Ruleset merging
---------------

Apart from simply writing the CSS in a shorter format (i.e.
whitespace/color/shorthand compression), mincss attempts to restructure rule
sets into a shorter variant. For example:

    a { color: red }
    p { color: red }

can be written much shorter as:

    a, p { color: red }

Merging selectors is something that is done by the programmer in most cases,
but the example above may occur when multiple different CSS files are merged
into one, or when a large CSS file is structured in such a way that the
definitions of `a` and `p` are far apart. A special case is when the same
selector appears in different rulesets, which may happen when a framework
stylesheet is merged with a page-specific stylesheet:

    a {
        color: blue;
        font-weight: bold;
    }

    ...

    a {
        color: red;
    }

This is merged into:

    a {
        color: blue;
        font-weight: bold;
        color: red;
    }

Because the `color` property is now overwritten in the same ruleset, the early
definition is removed:

    a {
        font-weight: bold;
        color: red;
    }
