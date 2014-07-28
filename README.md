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
number of phases:

1. `parse`: Transform the input into an Abstract Syntax Tree (AST) in
   accordance with the CSS syntax definition. This eliminates unnecessary
   whitespaces and catches syntax errors.
2. `unfold`:
   - Transform shorthand declarations into a separate declaration for each
     expression in the shorthand.
   - Duplicate rulesets for each of its selectors, so that every ruleset has
     exactly one block of declarations.
3. `concat`: Concatenate different declaration blocks for the same selector.
   Now, every selector has exacly one block of declarations associated with it.
4. `optimize`: Optimize individual declaration blocks by generating shorthand
   declarations and simple compression on expressions (color compression,
   removal of unnecessary string quotes, etc.).
5. `combine`: Try to combine declarations for different selectors, by comparing
   each pair of rulesets and checking if a shorter representation is possible
   by generating a shared ruleset.

Motivation
----------

The use of a proper CSS syntax definition for the parser, rather than a simple
ad-hoc tokenizer, assures that all unnecessary whitespace is ignored. The AST
definition also provides a convenient infrastructure for the subsequent
transformations.

The unfolding phase eliminates any clever constructions used by the programmer,
so that the following phases can make assumptions about the format of the
stylesheet and need not deal with, for example, shorthand declarations.

Concatenation of declaration blocks for the same selector is necessary in order
to generate a single minimal representation for each selector in the `optimize`
phase.

The `combine` phase is tricky to implement, since it is difficult to prove if
the resulting combinations are in fact the shortest possible representation of
the stylesheet. If this is not the case, the risk exists that while the
programmer efficiently merged some declarations for different selectors, the
compressor unfolds the ruleset and generates somethning longer.
*FIXME*: how do we solve this?


Building mincss
===============

TODO


Compression scores
==================

TODO: compare to existing minifiers
