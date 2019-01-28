# SchemeParser
This project follows the tutorial "Write Yourself A Scheme in 48 hours"
[https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours]

* This is a stack project, generated using intellij haskell plugin
* `stack ghci` runs the repl (on terminal)
* `stack run <args>` runs the main module
* `stack install parsec`
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
* `>>=` is a bind. >>= is used when you want to use the previous result in the next action.
* `>>` is also bind. Is used when you want to throw the result of prvs action.
* referential transparency: hs behaves like ordinary algebraic equations, you can substitute right side with left side, it evaluates to the same thing
* `return` wrap a function in a monad. let `a` be an ordinary function, `return a` can wrap it as IO
* `++` is list concatenation, can be used to concatenate strings
* hs compiles to hi files, and then to exe

#### Chapter 2
* hiding: `hiding` keyword skips the functions/identifiers. This is helpful when you want to overwrite some implementation by your own
* `parse` in `readExpr` returns `Either ParseError a`. `Left` and `Right`. Error handling, Left means you handle an error, and right means you continue
* This is handled using `case of...` expression, when you call a function that returns `Either <something>`
* `data` declaration is used to create Algebraic Data Types. An ADT simply can be used to create a type by combining new types, but it has some more theory behind it.
* `ADT` can be formed using algebraic operations, that is Sum or Product. e.g.
```
    List a = Nil | Cons List a
```
* Here Sum is `|` and product is `Cons List`
* `$` is infix function application, eliminate parens
* `liftM` operates on value wrapped inside Parser (refer `parseNumber`)
* `liftM :: Monad m => (a->b) -> m a -> m b` (same as `map` for list and `fmap` for functors)
* `compose (.) :: (b->c) -> (a->b) -> (a->c)`. It is similar to `(f(g(x)))` first x is computed on g and then f.


#### Chapter 3
* Pattern Matching


