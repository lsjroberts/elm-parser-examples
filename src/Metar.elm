module Main exposing (main)

import Browser exposing (sandbox)
import Char
import Debug
import Element exposing (..)
import Element.Font as Font
import Parser exposing (..)
import Parser.Extra exposing (..)



---


metar =
    "EGLL 232120Z 09014KT 260V320 9999 NCD 14/10 Q1030 NOSIG"


type alias Report =
    { station : String
    , time : Time
    , wind : WindInfo
    , remainder : String
    }


type alias Time =
    { day : Int
    , hour : Int
    , minute : Int
    }


type alias WindInfo =
    { dir : Int
    , speed : Int
    , gusts : Maybe Int
    }



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
        el [ centerX, centerY, Font.size 36 ]
            viewReport


viewReport =
    column [ spacing 50, Font.color (rgb 0.3 0.3 0.3) ]
        [ row [ spacing 10 ] [ text "METAR:", text metar ]
        , case parseReport metar of
            Ok report ->
                column [ spacing 30 ]
                    [ row [ spacing 10 ]
                        [ text "Station:"
                        , el [ Font.bold, Font.color (rgb255 0 168 112) ] <|
                            text report.station
                        ]
                    , row [ spacing 10 ]
                        [ text "Time:"
                        , el [ Font.bold, Font.color (rgb255 0 183 229) ] <|
                            text (Debug.toString report.time)
                        ]
                    , row [ spacing 10 ]
                        [ text "Wind:"
                        , el [ Font.bold, Font.color (rgb255 255 0 79) ] <|
                            text (Debug.toString report.wind)
                        ]
                    , row [ spacing 10 ] [ text "Remainder:", text report.remainder ]
                    ]

            Err deadEnds ->
                column [ spacing 30, Font.color (rgb255 255 0 79) ]
                    [ text "Error"
                    , text (Parser.Extra.deadEndsToString deadEnds)
                    ]
        ]



---


parseReport : String -> Result (List DeadEnd) Report
parseReport str =
    Parser.run reportParser str


reportParser : Parser Report
reportParser =
    succeed Report
        |= stationParser
        |. spaces
        |= timeParser
        |. spaces
        |= windInfoParser
        |. spaces
        |= remainderParser


stationParser : Parser String
stationParser =
    getChompedString <|
        chompWhile Char.isAlpha


timeParser : Parser Time
timeParser =
    succeed Time
        |= Parser.map toInt (numbers 2)
        |= Parser.map toInt (numbers 2)
        |= Parser.map toInt (numbers 2)
        |. token "Z"


windInfoParser : Parser WindInfo
windInfoParser =
    succeed
        (\dir speed maybeGust unit ->
            WindInfo dir (toMPS unit speed) maybeGust
        )
        |= Parser.map toInt (numbers 3)
        |= Parser.map toInt (oneOf [ backtrackable (numbers 3), numbers 2 ])
        |= oneOf [ Parser.map String.toInt gustParser, succeed Nothing ]
        |= oneOf
            [ Parser.map (\_ -> "KT") (token "KT")
            , Parser.map (\_ -> "MPS") (token "MPS")
            ]


gustParser : Parser String
gustParser =
    succeed identity
        |. token "G"
        |= oneOf [ backtrackable (numbers 3), numbers 2 ]


remainderParser : Parser String
remainderParser =
    getChompedString <|
        succeed identity
            |= chompWhile (\c -> True)



---


toInt =
    Maybe.withDefault 0 << String.toInt


toMPS : String -> Int -> Int
toMPS unit speed =
    case unit of
        "KT" ->
            speed // 2

        _ ->
            speed
