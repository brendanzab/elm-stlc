module STLC.ReadBack exposing (readBack)

import STLC.Local as Local
import STLC.Semantics as Semantics
import STLC.Syntax as Syntax


readBack : Local.Size -> Semantics.Value -> Syntax.Term
readBack _ value =
    case value of
        Semantics.Elim head spine ->
            Debug.todo "oops"

        Semantics.AtomTerm atom ->
            Syntax.AtomTerm atom

        Semantics.IntTerm int ->
            Syntax.IntTerm int

        Semantics.FunTerm closure ->
            Debug.todo "oops"
