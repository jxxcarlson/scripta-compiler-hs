# scripta-compiler-hs

A start on a compiler for Scripta in Haskell — analagous to the Elm compiler for Scripta;
This will take a while. For the moment, the compiler will be limited to handling
L0 markup text.  MicroLaTeX and XMarkdown will be added later.  Meanwhile, to 
experiment, run

```
stack run html inputs/ex3.txt >f.html
```


The ultimate
goal is to build a desktop version of scripta: integrated editor + Scripta compiler.
Possibly using monomer: https://github.com/fjvallarino/monomer.  This will have to
wait until ghc 9.2.4 builds on my M1 laptop under stack.  Aargh!

See https://scripta.io for the current web version (Elm + Lamdera).

Progress so far:

- Implement parser for primitive blocks (9/4/2022, 456 loc)
- Simplified tokenizer for L0 (9/6/2022, 597 loc)
- L0 parser, first draft: [L0Token] -> [Expr] (9/7/2022, 1062 loc)
- Add Parser.ExprBlock, change Main to parse file contents to [ExprBlock], then display the result (9/8/2022, 1224 loc)
- Add benchmarks. Add infrastructure for rendering web pages. (9/8/2022, 1270 loc)
- Limited rendering (text, i, b, red, blue and highlight elements, math text $ .. $ using KaTeX) (9/8/2022, 1291 loc)
- Rationalize handling of field 'properties' for PrimitiveBlock and ExprBlock (9/9/2022)
- Rationalize construction of the html header. See module Render.Block, which exports 'scriptaPage' (9/10/2022)
