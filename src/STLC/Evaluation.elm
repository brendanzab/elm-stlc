module STLC.Evaluation exposing (eval)

import STLC.Local as Local
import STLC.Semantics as Semantics
import STLC.Syntax as Syntax


eval : Local.Env Semantics.Value -> Syntax.Term -> Semantics.Value
eval _ _ =
    Debug.todo "oops"
