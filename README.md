### A C Compiler

This is a C compiler. It accepts all C programs which consist of a single
function with a single return statement returning a literal integer.

    cabal v2-run acc test/two.c
    test/two
    echo $?
