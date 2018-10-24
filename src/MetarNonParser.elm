module Main exposing (main)

import Browser exposing (sandbox)
import Element exposing (..)
import Element.Font as Font
import Regex



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
            (viewWindSpeed "09014KT")



---


viewWindSpeed windInfo =
    case windSpeedWithString windInfo of
        Just speed ->
            text (String.fromInt speed)

        Nothing ->
            text "Error"



---


windSpeedWithString : String -> Maybe Int
windSpeedWithString windInfo =
    let
        -- Remove the wind direction
        speed =
            String.dropLeft 3 windInfo
    in
    -- Remove the "knots" unit and read the number
    speed
        |> String.dropRight (String.length speed - 2)
        |> String.toInt



---


windSpeedPattern =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[0-9]{3}([0-9]{2,3})(KT|MPS)"


windSpeedWithRegex : String -> Maybe Int
windSpeedWithRegex windInfo =
    let
        matches =
            List.map .submatches (Regex.find windSpeedPattern windInfo)
    in
    case matches of
        [ speed :: unit ] ->
            speed |> Maybe.andThen String.toInt

        _ ->
            Nothing
