module STLC.Semantics exposing
    ( Closure
    , Elim(..)
    , Head(..)
    , Value(..)
    )

import STLC.Local as Local
import STLC.Syntax as Syntax


{-| Values for the simply typed lambda calculus

```text
t ::= 'atom
    | n
    | t : T
    | x
    | \x -> t
    | t1 t2
```

-}
type Value
    = Elim Head (List Elim)
    | AtomTerm String
    | IntTerm Int
    | FunTerm Closure


type alias Closure =
    { env : Local.Env Value
    , term : Syntax.Term
    }


type Head
    = Local Local.Level


type Elim
    = FunctionElim Value
