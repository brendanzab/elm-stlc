-- Copyright Brendan Zabarauskas, 2018
--
-- Released under the MIT license


module Main exposing (main)

{-| A type checker for the simply typed lambda calculus, implemented in a
bidirectional style.

The point of this is to show that type checking is actually quite simple if
you follow a bidirectional approach! Hopefully this whets your appetite for
playing around with your own type checkers!

Further reading:

  - <http://www.davidchristiansen.dk/tutorials/bidirectional.pdf>
  - <http://davidchristiansen.dk/tutorials/nbe/>
  - <https://www.andres-loeh.de/LambdaPi/>

-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, input, li, p, pre, text, ul)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser, keyword, lazy, oneOf, spaces, succeed, symbol)
import Set



-- SYNTAX


{-| Types in the simply typed lambda calculus

```text
T ::= Atom
    | Int
    | T1 -> T2
```

-}
type Type
    = AtomType
    | IntType
    | FunType Type Type


{-| Terms in the simply typed lambda calculus

```text
t ::= 'atom
    | n
    | t : T
    | x
    | \x -> t
    | t1 t2
```

-}
type Term
    = Atom String
    | IntLiteral Int
    | Ann Term Type
    | Var String
    | Lam String Term
    | App Term Term


{-| A context for remembering the types of variables during type checking
-}
type alias Context =
    Dict String Type


emptyContext : Context
emptyContext =
    Dict.empty


lookupContext : String -> Context -> Maybe Type
lookupContext =
    Dict.get


extendContext : String -> Type -> Context -> Context
extendContext =
    Dict.insert



-- EXAMPLE TERMS


exampleId : Term
exampleId =
    -- \x -> x
    Lam "x" (Var "x")


exampleAlwaysHello : Term
exampleAlwaysHello =
    -- \_ -> 'hello
    Lam "_" (Atom "hello")


exampleAlways : Term
exampleAlways =
    -- (\x _ -> x) : Atom -> Atom -> Atom
    Ann (Lam "x" (Lam "_" (Var "x"))) (FunType AtomType (FunType AtomType AtomType))



-- TYPE CHECKING


{-| Infer the type of a term in a context
-}
infer : Context -> Term -> Result String Type
infer context term =
    case term of
        Atom _ ->
            Ok AtomType

        IntLiteral value ->
            Ok IntType

        Ann annedTerm ty ->
            check context annedTerm ty
                |> Result.map (always ty)

        Var name ->
            case lookupContext name context of
                Just ty ->
                    Ok ty

                Nothing ->
                    Err ("undefined variable called `" ++ name ++ "`")

        Lam paramName _ ->
            Err ("lambda needs type annotation for the parameter called `" ++ paramName ++ "`")

        App fnTerm argTerm ->
            infer context fnTerm
                |> Result.andThen
                    (\ty ->
                        case ty of
                            FunType paramTy returnTy ->
                                check context argTerm paramTy
                                    |> Result.map (always returnTy)

                            fnTy ->
                                Err
                                    ("expected function type, found `"
                                        ++ tyToString fnTy
                                        ++ "`"
                                    )
                    )


{-| Check that a term conforms to a given type with respect to a context
-}
check : Context -> Term -> Type -> Result String ()
check context term expectedTy =
    case ( term, expectedTy ) of
        ( Lam paramName bodyTerm, FunType paramTy returnTy ) ->
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
                                    ++ tyToString expectedTy
                                    ++ "` but found `"
                                    ++ tyToString inferredTy
                                    ++ "`"
                                )
                    )



-- STRING CONVERSIONS


{-| Convert a type to a string
-}
tyToString : Type -> String
tyToString ty =
    case ty of
        AtomType ->
            "Atom"

        IntType ->
            "Int"

        FunType paramTy returnTy ->
            "(" ++ tyToString paramTy ++ " -> " ++ tyToString returnTy ++ ")"


{-| Convert a term to a string
-}
termToString : Term -> String
termToString term =
    case term of
        Atom atom ->
            "'" ++ atom

        IntLiteral value ->
            String.fromInt value

        Ann annedTerm ty ->
            "(" ++ termToString annedTerm ++ " : " ++ tyToString ty ++ ")"

        Var name ->
            name

        Lam paramName body ->
            "(\\" ++ paramName ++ " => " ++ termToString body ++ ")"

        App fnTerm argTerm ->
            "(" ++ termToString fnTerm ++ " " ++ termToString argTerm ++ ")"



-- PARSING


varParser : Parser String
varParser =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "Atom", "Int" ]
        }

intParser : Parser Int
intParser =
    Parser.number
        { int = Just identity
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Nothing
        }


atomicTyParser : Parser Type
atomicTyParser =
    oneOf
        [ succeed AtomType
            |. keyword "Atom"
        , succeed IntType
            |. keyword "Int"
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> tyParser)
            |. spaces
            |. symbol ")"
        ]


tyParser : Parser Type
tyParser =
    let
        tyHelp lhsTy =
            oneOf
                [ succeed (FunType lhsTy)
                    |. spaces
                    |. symbol "->"
                    |. spaces
                    |= lazy (\_ -> tyParser)
                    |. spaces
                , succeed lhsTy
                ]
    in
    atomicTyParser |> Parser.andThen tyHelp


atomicTermParser : Parser Term
atomicTermParser =
    oneOf
        [ succeed Atom
            |. symbol "'"
            |= varParser
        , succeed IntLiteral
            |= intParser
        , succeed Var
            |= varParser
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> termParser)
            |. spaces
            |. symbol ")"
        ]


appTermParser : Parser Term
appTermParser =
    let
        termHelp appTerms =
            oneOf
                [ succeed (\argTerm -> Parser.Loop (App appTerms argTerm))
                    |. spaces
                    |= atomicTermParser
                , succeed ()
                    |> Parser.map (\_ -> Parser.Done appTerms)
                ]
    in
    (atomicTermParser |. spaces)
        |> Parser.andThen (\term -> Parser.loop term termHelp)


lamTermParser : Parser Term
lamTermParser =
    oneOf
        [ appTermParser
        , succeed Lam
            |. symbol "\\"
            |. spaces
            |= varParser
            |. spaces
            |. symbol "=>"
            |. spaces
            |= lazy (\_ -> lamTermParser)
        ]


termParser : Parser Term
termParser =
    let
        termHelp lhsTerm =
            oneOf
                [ succeed (Ann lhsTerm)
                    |. spaces
                    |. symbol ":"
                    |. spaces
                    |= lazy (\_ -> tyParser)
                    |. spaces
                , succeed lhsTerm
                ]
    in
    lamTermParser |> Parser.andThen termHelp



-- APPLICATION


type alias Model =
    { src : String
    , inferred : Maybe (Result String Type)
    }


initialModel : Model
initialModel =
    { src = ""
    , inferred = Nothing
    }


type Msg
    = UpdateSrc String
    | TypeCheck


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSrc newSrc ->
            { model | src = newSrc }

        TypeCheck ->
            let
                inferred =
                    model.src
                        |> Parser.run (termParser |. spaces |. Parser.end)
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen (infer emptyContext)
            in
            { model | inferred = Just inferred }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "My lil typechecker \u{1F970}" ]
        , p [] [ text "An example of type checking the simply typed lambda calculus." ]
        , p [] [ text "Some example terms to try:" ]
        , ul []
            [ li [] [ pre [] [ text "'x" ] ]
            , li [] [ pre [] [ text "'x : Atom" ] ]
            , li [] [ pre [] [ text "1" ] ]
            , li [] [ pre [] [ text "\\x => x : Atom -> Atom" ] ]
            , li [] [ pre [] [ text "(\\x => x : Atom -> Atom) 'x" ] ]
            , li [] [ pre [] [ text "(\\x => \\y => x : Atom -> Atom -> Atom) 'x" ] ]
            ]
        , p []
            [ input [ onInput UpdateSrc ] [ text model.src ]
            , button [ onClick TypeCheck ] [ text "infer" ]
            ]
        , pre []
            (case model.inferred of
                Nothing ->
                    []

                Just (Ok ty) ->
                    [ text ("inferred: " ++ tyToString ty) ]

                Just (Err msg) ->
                    [ text ("😬 - " ++ msg) ]
            )
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
