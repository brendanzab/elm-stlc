module STLC.Evaluation exposing (eval)

import STLC.Local as Local
import STLC.Semantics as Semantics
import STLC.Syntax as Syntax


eval : Local.Env Semantics.Value -> Syntax.Term -> Semantics.Value
eval env term =
    case term of
        Syntax.Ann annedTerm _ ->
            eval env annedTerm

        Syntax.Local local ->
            Debug.todo "oops"

        Syntax.AtomTerm atom ->
            Semantics.AtomTerm atom

        Syntax.IntTerm int ->
            Semantics.IntTerm int

        Syntax.FunTerm _ bodyTerm ->
            Debug.todo "oops"

        Syntax.FunElim headTerm argumentTerm ->
            Debug.todo "oops"
