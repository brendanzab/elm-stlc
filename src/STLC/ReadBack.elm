module STLC.ReadBack exposing (readBack)

import STLC.Local as Local
import STLC.Semantics as Semantics
import STLC.Syntax as Syntax


readBack : Local.Size -> Semantics.Value -> Syntax.Term
readBack _ _ =
    Debug.todo "oops"
