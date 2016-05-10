# haskal-interpreter

This is a project for Programming Languages and Paradigms course held in 2016 at Faculty of Mathematics, Informatics, and Mechanics of Warsaw University.

The interpreter accepts programs in simple Pascal-inspired language (codenamed Haskal).


## Haskal language

Some of the more notable language features include:
* two value types: `int` and `bool`
* procedures with arguments passed by value and by name
* full identifier shadowing, including Pascal-like nested procedure declarations
* several helpful operators borrowed from C like `+=` and `++`
* built-in `print` statement

Examples demonstrating the language and interpreter capabilities can be found in `good` and `bad` directories.


## Requirements
* ghc
* happy
* alex


## Building the interpreter

Just call `make`.


## Using the interpreter

The interpreter can run code from a file on path passed as the first argument or, if no arguments are present, from standard input.

The interpreter can be launched via `./interpreter`.


## Notes about the interpreter output

Currently, the output is buffered and printed only after the program has terminated. It will also not be printed if an error occurs during execution, so it cannot be used for debugging. This should be changed in future versions.
