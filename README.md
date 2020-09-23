This collection of files will produce a Haskell executable
that takes as input the source code of a valid C program
from the `chrony` codebase and a function within that C
program, and outputs the source code necessary for ALPACA
to perform a modular analysis of that function.

This project is closely related to ALPACA, but is its own
beast, so it'll be called `llama`. To produce the `llama` 
executable, run `make` within this repo's base directory.
To give the tests a go, run `stack test` within this repo's
base directory.

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

(TODO: import `chrony` codebase to set up automated tests 
on shared, stable copies.)

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