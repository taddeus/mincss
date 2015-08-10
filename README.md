About
=====

mincss is an extendible CSS minifier written in OCaml. It features a complete
parser for the CSS3 language, along with type definitions that are consistent
with the official CSS specification and a traversal utility function for use in
transformation passes.


Installation
============

For now, there is no easy installation option for mincss (yet). An up-to-date
64-bit ELF binary is available for download
[here](http://mincss.tkroes.nl/mincss). You can also build the binary from
source (see *Building mincss* below).


Features
========

Whitespace compression
----------------------

    a,                                  |  a,.myclass [class~="foo"]>p{color:#fff}
    .myclass [class ~= "foo"] >  p {    |
        color: #fff;                    |
    }                                   |

Compression of simple expressions
---------------------------------

    color: rgb(257, -2, 0);             |  color: red;
    color: rgb(67%, 67.5%, 68%);        |  color: #abacad;
    color: #aaffbb;                     |  color: #afb;
    color: white;                       |  color: #fff;
    font-weight: normal;                |  font-weight: 400;

Generation of shorthand properties
----------------------------------

    font-weight: normal;                |  font: normal 12px/15px sans-serif;
    font-size: 12px;                    |
    line-height: 15px;                  |
    font-family: sans-serif;            |

Any existing shorthands are first unfolded into their non-shorthand
counterparts to be folded back later. This means that if `--duplicates` is
enabled, the last value is used for shorthand generation, merging separate :

    font: normal 12px/15px sans-serif;  |  font: bold 12px/15px sans-serif;
    font-weight: bold;                  |

Pruning duplicate declarations
------------------------------

    color: #000;                        |  color: #fff;
    color: #fff;                        |

Note that `!important` annotations are correctly handled:

    color: #eee !important;             |  color: #000 !important;
    color: #000 !important;             |
    color: #fff;                        |

Sorting declarations
--------------------
The `--sort` command-line option sorts declarations alphabetically. This option
is disabled by default since it does not affect file size.


Command-line interface
======================
Output of `mincss -h`:

    Usage: mincss [<options>] [<file> ...]

    Generic options:
     -h, --help        Show this help message
     -v, --verbose     Verbose mode: show compression rate
     -o <file>         Output file (defaults to stdout)
     <file> ...        Input files (default is to read from stdin)

    Optimization flags (default is -w -c -s -d):
     -w, --whitespace  Eliminate unnecessary whitespaces (has the greatest effect, omit for pretty-printing)
     -c, --simple      Shorten colors, font weights and nth-child
     -s, --shorthands  Generate shorthand properties
     -d, --duplicates  Prune duplicate properties (WARNING: may affect cross-browser hacks)
     -p, --pretty      Shorthand for -c -s -d

    Formatting options:
     -r, --sort        Sort declarations in each ruleset (always on when --shorthands is enabled)
     -e, --echo        Just parse and pretty-print, no optimizations


Building mincss
===============

Dependencies are [OCaml](https://ocaml.org/docs/install.html) 4.0 and
[menhir](http://cristal.inria.fr/~fpottier/menhir/).

Bootstrapping on a Debian system can be done as follows:

    $ sudo apt-get install ocaml menhir git
    $ git clone git@github.com:taddeus/mincss.git
    $ cd mincss
    $ make
    $ ./mincss --help

I prefer to use [Opam](https://opam.ocaml.org/) myself because it offers more
flexibility:

    $ sudo apt-get install opam
    $ opam init
    $ eval `opam config env`
    $ opam switch 4.02.0
    $ opam update
    $ opam install menhir


TODO / bugs
===========

- `border` shorthand generation produces out-of-order results when
  direction-specific declarations follow a generic border declaration. This
  produces inequivalent CSS, and could be fixed by unfolding each generic
  border declaration into four direction-specific ones, and sybsequently
  generating the shortest possible representation of the resulting box model.
  Edit: a better/easier solution is to generate shorthand properties at the
  index of the first merged declaration. This might not work in all cases but
  it is better than putting everything at the end.
- `border:none` could be `border:0`, or in general any shorthand that has both
  a `style` and `width` property should be transformed from `none` into `0`.
