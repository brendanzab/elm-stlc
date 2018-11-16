# elm-stlc

A type checker for the simply typed lambda calculus, implemented in a
bidirectional style.

The point of this is to show that type checking is actually quite simple if
you follow a bidirectional approach! Hopefully this whets your appetite for
playing around with your own type checkers!

The idea behind bidirectional type checking is that you split up your syntax 
into terms that are:

- easy to infer the type of
- need additional type annotations to check them

For example:

- `'foo` is easy to infer - it's an `Atom`
- `\x => x` can't have its type inferred - it needs an annotation

This split is reflected in the type checker, which has two mutually defined functions:

```elm
infer : Context -> Term -> Result String Type
check : Context -> Term -> Type -> Result String ()
```

## Running the app

To play with this, install Elm 0.19, clone the repository, and run the Elm
Reactor:

```
elm reactor
```

Then open `src/Main.elm` in the file navigator

## Further reading

- http://www.davidchristiansen.dk/tutorials/bidirectional.pdf
- http://davidchristiansen.dk/tutorials/nbe/
- https://www.andres-loeh.de/LambdaPi/

