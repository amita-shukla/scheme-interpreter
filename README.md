# SchemeParser
This project follows the tutorial "Write Yourself A Scheme in 48 hours"
[https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours]

* This is a stack project, generated using intellij haskell plugin
* `stack ghci` runs the repl (on terminal)
* `stack run <args>` runs the main module
* To import a library `parsec`, make changes in package.yaml `dependencies`
* Do not touch the cabal file, as it is auto generated

## Notes

#### Haskell Styling
* Tabs are illegal.
* types with caps and functions with lower
* a `do block` consists of a series of lines, all lined up with the first non-whitespace character after the `do`.

#### Chapter 1
* Haskell is declarative.
* A haskell program is a collection of definitions
* These definitions are a collection if actions and functions.
* Monads? An action, computation builder, e.g. list comprehemsion, IO, Parser (like one we're building here) etc.
* In a `do-block`, every computation is wrapped in a separate anonymous function, which are combined together using bind.
* `>>=` is a bind. Use this when you need the result of the previous computation
* `>>` is also bind. Use this to sequence the operations in a `do` without storing the result.
* referential transparency: hs behaves like ordinary algebraic equations, you can substitute right side with left side, it evaluates to the same thing
* `return` wrap a function in a monad. let `a` be an ordinary function, `return a` can wrap it as IO
* `++` is list concatenation, can be used to concatenate strings
* hs compiles to hi files, and then to exe

#### Chapter 2






