---
title: Assignment 2 - BNF2Haskell
author: FIT2102 Programming Paradigms
margin: 1inch
---

Please do not change the names of the functions defined in the Assignment.hs file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the Haskell files using the sample input BNF files, by running your code for each exercise.

All example BNF files are stored within `examples/input` and the output of your parser will be saved in `examples/output`.

## Running the Interactive Page

In the Haskell folder run:

```
$ stack run
```

In a separate terminal, in the javascript folder run:

```
$ npm i
$ npm run dev
```

You can type BNF in to the LHS of the webpage and inspect the converted Haskell.

## What was modified
- Main.ts
    - Added a function to save the generated haskell parser (Line 54-85 and Line 138 and Line 158-164)
- Main.hs
    - Added a new route pattern in Main.hs (Line 188-204) that can be called by the Main.ts
    and save a the generated Haskell Code
- type.ts
    - Added a new save parameter in the state
- Assignment.hs
    - Implemented the ADT for parser
    - Added the Haskell Code generation from the parser
- index.html
    - Added a save button (Line 53)