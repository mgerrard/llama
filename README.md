# LLAMA

## About
LLAMA (Lessen Loaded ASTs (for) Modular Analysis) is a preprocessing tool used to produce 
abbreviated C programs to allow for modular analysis of C programs. LLAMA accepts a C
program, `P`, and a function, `f()`, in said program and produces an C program, `P'`,
that contains the portions of the program needed to analyze `f()`. This project is closely 
related to ALPACA, but is its own beast, so it'll be called `llama`. 

## Build
To produce the `llama` executable, run `make` within this repo's base directory.
To give the tests a go, run `stack test` within this repo's base directory.

## Usage
`llama <foo.c> <bar> [stub.c] [-debug]`

Llama requires two arguments and allows for an optional third argument. The first argument is
the program that will be pared down and the second is the function that will be analyzed.
Llama attempts to create a minimal program that analyzers can process. To this end, many 
libraries may not be pulled into the program which may prevent parts of the program to be 
analyzed. To remedy this, llama accepts a stub file which contains definition for the functions
that are not already defined. Llama also allows for the -debug flag to allow for debugging of the output.

## Misc

Two technical notes that relevant at a high level:
(1) we use the `language-c` Haskell library to get ahold of
the abstract syntax tree of the input C program--the API for
the AST can be found 
[here](https://hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Syntax-AST.html); 
and (2) this project will be
considered semi-stable when all 12 functions of depth 0
(depth 0 means the function calls `assert`; depth 1 calls a 
depth 0 function, etc.) in the call graphs of functions in
the `chrony` codebase can be successfully run through 
ALPACA, according to some oracle.

Chrony 0th Cases:
In array.c:

*  `ARR_CreateInstance`
*  `realloc_array` (includes an undeclared function `Realloc2`; when this and the while loop are commented out, CIVL says there's no valid evidence; will need to try with CPA-SymExec)
*  `ARR_GetElement` (can make it through with `alpaca -p cpaSeq pruned_file.c`)
*  `ARR_GetElements`

In util.c:

*  `UTI_FloatHostToNetwork` 
*  `UTI_GetNtp64Fuzz`

In client.c:

*  `request_reply` (throw out: includes asm in called function (`submit_request`))
*  `submit_request` (throw out: includes asm in macro expansion of `FD_ZERO`)
*  `parse_allow_deny`
*  `print_report`
*  `bits_to_mask` (how should we make the union field of `IPAddr` symbolic?)
*  `prepare_socket` (also deals with a union: `sockaddr_all`)
