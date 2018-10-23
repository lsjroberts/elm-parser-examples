module Main exposing (main)

import Browser exposing (sandbox)
import Char
import Debug
import Element exposing (..)
import Element.Font as Font
import Parser exposing (..)
import Parser.Extra exposing (..)



---


main =
    sandbox
        { init = init
        , update = update
        , view = view
        }


init =
    0


update msg model =
    model


view model =
    layout [] <|
        el [ centerX, centerY, Font.size 24 ]
            viewReport


viewReport =
    case parse "123456" of
        Ok report ->
            text (Debug.toString report)

        Err deadEnds ->
            column [ spacing 30 ]
                [ text "Error"
                , text (Parser.Extra.deadEndsToString deadEnds)
                ]



---


parse : String -> Result (List DeadEnd) (List Int)
parse str =
    Parser.run numbersParser str


numbersParser : Parser (List Int)
numbersParser =
    succeed
        (\one two three -> [ toInt one, toInt two, toInt three ])
        |= numbers 1
        |= numbers 2
        |= numbers 3


toInt =
    Maybe.withDefault 0 << String.toInt
