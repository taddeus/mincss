About
=====

mincss is an extendible CSS minifier written in OCaml. It features a complete
parser for the CSS3 language, along with type definitions that are consistent
with the official CSS specification and a traversal utility function for use in
transformation passes.


Optimizations
=============

Whitespace compression
----------------------

    a,                                  |  a,.myclass [class~="foo"]>p{color:#fff}
    .myclass [class ~= "foo"] >  p {    |
        color: #fff;                    |
    }                                   |

Compression of simple expressions
---------------------------------

    color: white;                       |  color: #fff;
    font-weight: normal;                |  font-weight: 400;

Generation of shorthand properties
----------------------------------

    font-weight: normal;                |  font: normal 12px/15px sans-serif;
    font-size: 12px;                    |
    line-height: 15px;                  |
    font-family: sans-serif;            |

Any existing shorthands are first unfolded into their non-shorthand
counterparts, after which the last value is used for shorthand generation:

    font: normal 12px/15px sans-serif;  |  font: bold 12px/15px sans-serif;
    font-weight: bold;                  |


Command-line interface
======================
Output of `mincss -h`:

    Usage: ./mincss [<options>] [<file> ...]

    Generic options:
     -h, --help        Show this help message
     -v, --verbose     Verbose mode: show compression rate
     -o <file>         Output file (defaults to stdout)
     <file> ...        Input files (default is to read from stdin)

    Optimization flags (if none are specified, all are enabled):
     -w, --whitespace  Eliminate unnecessary whitespaces (has the greatest effect, omit for pretty-printing)
     -c, --simple      Shorten colors and font weights
     -s, --shorthands  Generate shorthand properties
     -d, --duplicates  Prune duplicate properties (WARNING: may affect cross-browser hacks)
     -p, --pretty      Shorthand for -c -s -d
     -e, --echo        Just parse and pretty-print, no optimizations

    Formatting options:
     --sort            Sort declarations in each selector group


Building mincss
===============

Dependencies are [OCaml](https://ocaml.org/docs/install.html) 4.0 and
[menhir](http://cristal.inria.fr/~fpottier/menhir/).

Bootstrapping on a Debian system can be done as follows:

    $ sudo apt-get install ocaml opam git
    $ opam init
    $ opam switch 4.01.0
    $ opam install menhir
    $ git clone git@github.com:taddeus/mincss.git
    $ cd mincss
    $ make
    $ ./mincss --help
