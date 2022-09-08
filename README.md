# scripta-compiler-hs

A start on a compiler for Scripta in Haskell.  Will be analagous to the Elm compiler for Scripta;
will try some experiments and try to make improvements. This will take a while.

See https://scripta.io for the current Elm version.

Progress so far:

- Implement parser for primitive blocks (9/4/2022, 456 loc)
- Simplified tokenizer for L0 (9/6/2022, 597 loc)
- L0 parser, first draft: [L0Token] -> [Expr] (9/7/2022, 1062 loc)
- Add Parser.ExprBlock, change Main to parse file contents to [ExprBlock], then display the result (9/8/2022, 1224 loc)
- Add benchmarks. (9/8/2022)
