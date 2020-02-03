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
import Html exposing (Html, button, div, h1, input, li, p, pre, text, ul)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=))
import STLC.Syntax as Syntax
import STLC.Typing as Typing



-- APPLICATION


type alias Model =
    { typingContext : Typing.Context
    , src : String
    , inferred : Maybe (Result String ( Syntax.Term, Syntax.Type ))
    }


initialModel : Model
initialModel =
    { typingContext = Typing.emptyContext
    , src = ""
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
                parser =
                    Parser.succeed identity
                        |. Parser.spaces
                        |= Syntax.termParser
                        |. Parser.spaces
                        |. Parser.end

                inferred =
                    model.src
                        |> Parser.run parser
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen
                            (\term ->
                                Typing.infer model.typingContext term
                                    |> Result.map (\ty -> ( term, ty ))
                            )
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

                Just (Ok ( term, ty )) ->
                    [ text (Syntax.termToString term ++ " : " ++ Syntax.tyToString ty) ]

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
