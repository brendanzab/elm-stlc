module STLC.Syntax exposing
    ( Term(..)
    , Type(..)
    , termParser
    , termToString
    , tyParser
    , tyToString
    )

import Parser exposing ((|.), (|=), Parser, keyword, lazy, oneOf, spaces, succeed, symbol)
import Set


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
    | IntTerm Int
    | Ann Term Type
    | Local String
    | FunTerm String Term
    | FunElim Term Term



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

        IntTerm value ->
            String.fromInt value

        Ann annedTerm ty ->
            "(" ++ termToString annedTerm ++ " : " ++ tyToString ty ++ ")"

        Local name ->
            name

        FunTerm paramName body ->
            "(\\" ++ paramName ++ " => " ++ termToString body ++ ")"

        FunElim headTerm argTerm ->
            "(" ++ termToString headTerm ++ " " ++ termToString argTerm ++ ")"



-- PARSING


nameParser : Parser String
nameParser =
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
            |= nameParser
        , succeed IntTerm
            |= intParser
        , succeed Local
            |= nameParser
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> termParser)
            |. spaces
            |. symbol ")"
        ]


funElimParser : Parser Term
funElimParser =
    let
        termHelp appTerms =
            oneOf
                [ succeed (\argTerm -> Parser.Loop (FunElim appTerms argTerm))
                    |. spaces
                    |= atomicTermParser
                , succeed ()
                    |> Parser.map (\_ -> Parser.Done appTerms)
                ]
    in
    (atomicTermParser |. spaces)
        |> Parser.andThen (\term -> Parser.loop term termHelp)


funTermParser : Parser Term
funTermParser =
    oneOf
        [ funElimParser
        , succeed FunTerm
            |. symbol "\\"
            |. spaces
            |= nameParser
            |. spaces
            |. symbol "=>"
            |. spaces
            |= lazy (\_ -> funTermParser)
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
    funTermParser |> Parser.andThen termHelp
