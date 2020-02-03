module STLC.Typing exposing
    ( Context
    , check
    , emptyContext
    , extendContext
    , infer
    , lookupContext
    )

import Dict exposing (Dict)
import STLC.Syntax as Syntax


{-| A context for remembering the types of variables during type checking
-}
type alias Context =
    Dict String Syntax.Type


emptyContext : Context
emptyContext =
    Dict.empty


lookupContext : String -> Context -> Maybe Syntax.Type
lookupContext =
    Dict.get


extendContext : String -> Syntax.Type -> Context -> Context
extendContext =
    Dict.insert


{-| Infer the type of a term in a context
-}
infer : Context -> Syntax.Term -> Result String Syntax.Type
infer context term =
    case term of
        Syntax.Atom _ ->
            Ok Syntax.AtomType

        Syntax.IntTerm _ ->
            Ok Syntax.IntType

        Syntax.Ann annedTerm ty ->
            check context annedTerm ty
                |> Result.map (always ty)

        Syntax.Local name ->
            case lookupContext name context of
                Just ty ->
                    Ok ty

                Nothing ->
                    Err ("undefined variable called `" ++ name ++ "`")

        Syntax.FunTerm paramName _ ->
            Err ("lambda needs type annotation for the parameter called `" ++ paramName ++ "`")

        Syntax.FunElim headTerm argTerm ->
            infer context headTerm
                |> Result.andThen
                    (\ty ->
                        case ty of
                            Syntax.FunType paramTy returnTy ->
                                check context argTerm paramTy
                                    |> Result.map (always returnTy)

                            fnTy ->
                                Err
                                    ("expected function type, found `"
                                        ++ Syntax.tyToString fnTy
                                        ++ "`"
                                    )
                    )


{-| Check that a term conforms to a given type with respect to a context
-}
check : Context -> Syntax.Term -> Syntax.Type -> Result String ()
check context term expectedTy =
    case ( term, expectedTy ) of
        ( Syntax.FunTerm paramName bodyTerm, Syntax.FunType paramTy returnTy ) ->
            let
                innerContext =
                    context |> extendContext paramName paramTy
            in
            check innerContext bodyTerm returnTy

        ( _, _ ) ->
            infer context term
                |> Result.andThen
                    (\inferredTy ->
                        if inferredTy == expectedTy then
                            Ok ()

                        else
                            Err
                                ("type mismatch, expected `"
                                    ++ Syntax.tyToString expectedTy
                                    ++ "` but found `"
                                    ++ Syntax.tyToString inferredTy
                                    ++ "`"
                                )
                    )
