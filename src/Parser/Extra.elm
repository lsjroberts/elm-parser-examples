module Parser.Extra exposing (count, deadEndsToString, numbers)

import Parser exposing (..)


numbers times =
    count times Char.isDigit


count times fn =
    -- TODO: Oh dear me.
    -- `countHelp` fails because it is greedy and takes the next char
    case times of
        1 ->
            getChompedString (chompIf fn)

        2 ->
            succeed (\a b -> String.join "" [ a, b ])
                |= getChompedString (chompIf fn)
                |= getChompedString (chompIf fn)

        3 ->
            succeed (\a b c -> String.join "" [ a, b, c ])
                |= getChompedString (chompIf fn)
                |= getChompedString (chompIf fn)
                |= getChompedString (chompIf fn)

        _ ->
            problem ("Unable to count to " ++ String.fromInt times)



-- loop [] (countHelp times fn)
--     |> Parser.map String.concat


countHelp times fn items =
    let
        checkTimes item =
            if String.length item > 0 && List.length items < times then
                Loop (item :: items)

            else
                Done (List.reverse items)
    in
    succeed checkTimes
        |= getChompedString (chompIf fn)



-- |= oneOf
--     [ getChompedString (chompIf fn)
--     , getChompedString (chompWhile (\_ -> True))
--     ]


deadEndsToString deadEnds =
    let
        problemToString problem =
            case problem of
                Expecting s ->
                    "Expecting: " ++ s

                ExpectingInt ->
                    "Expecting int"

                ExpectingHex ->
                    "Expecting hex"

                ExpectingOctal ->
                    "Expecting octal"

                ExpectingBinary ->
                    "Expecting binary"

                ExpectingFloat ->
                    "Expecting float"

                ExpectingNumber ->
                    "Expecting number"

                ExpectingVariable ->
                    "Expecting variable"

                ExpectingSymbol s ->
                    "Expecting symbol: " ++ s

                ExpectingKeyword k ->
                    "Expecting keyword: " ++ k

                ExpectingEnd ->
                    "Expecting end"

                UnexpectedChar ->
                    "Unexpected char"

                Problem s ->
                    "Problem: " ++ s

                BadRepeat ->
                    "Bad repeat"
    in
    deadEnds
        |> List.map
            (\{ row, col, problem } ->
                problemToString problem ++ " at " ++ String.fromInt row ++ ":" ++ String.fromInt col
            )
        |> String.join "\n"
