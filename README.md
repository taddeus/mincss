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
sets such that the resulting stylesheet is the minimal representation of the
input. For example:

    a { color: red }
    p { color: red }

can be written much shorter as:

    a, p { color: red }

Merging selectors is something that is done by the programmer in most cases,
but the example above may occur when multiple different CSS files are merged
into one, or when a large CSS file is structured in such a way that the
definitions of `a` and `p` are far apart. Another thing that may happen is that
some framework file defines a default style for a selector, that is then
overwritten in a custom stylesheet, for example:

    /* file: framework.css */
    a {
        border: 1px solid red;
    }

    ...

    /* file: my-special-page.css */
    a {
        border-color: blue;
    }

which can be merged into:

    a {
        border: 1px solid blue;
    }


Compression phases
==================

To achieve the features listed above, the input stylesheet is rewritten in a
number of steps:

1. Parse the input CSS, producing an Abstract Syntax Tree (AST) in accordance
   with the CSS syntax definition. This eliminates unnecessary whitespaces and
   catches syntax errors.
2. Transform shorthand declarations into a separate declaration for each
   expression in the shorthand.
3. Duplicate rulesets for each of its selectors, so that every ruleset has
   exactly one selector.
4. Create a new declaration block for each property declaration on each
   selector, e.g. `a,p{color:red;border:blue}` becomes `a{color:red}
   a{border:blue} p{color:red} p{border:blue}`.
5. Prune duplicate declarations (when the same property is overwritten for the
   same selector). Note: this may be disabled with `--no-prune` for stylesheets
   that contain duplication hacks.
6. Combine selectors for identical blocks (as long as the correct order of
   declarations is preserved).
7. Concatenate blocks for identical (combinations of) selectors.
8. Optimize individual declaration blocks by generating shorthand declarations
   and simple compression on expressions (i.e. color compression, removal of
   unnecessary string quotes, etc.).
9. Output the resulting AST with minimal whitespace.


Building mincss
===============

TODO


Compression scores
==================

TODO: compare to existing minifiers
