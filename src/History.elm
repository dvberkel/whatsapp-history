module History exposing (History, Problem(..), history, parse)

import Message exposing (Message, message, Timestamp, timestamp)
import Parser exposing (..)


parse : String -> Result Problem History
parse input =
    run historyParser input
        |> Result.mapError Parse


type Problem
    = General String
    | Parse (List DeadEnd)


type History
    = History
        { messages : List Message
        }


history : List Message -> History
history messages =
    History { messages = messages }


historyParser : Parser History
historyParser =
    Parser.map history <|
        sequence
            { start = ""
            , separator = "\n"
            , end = ""
            , spaces = succeed ()
            , item = messageParser
            , trailing = Optional
            }


messageParser : Parser Message
messageParser =
    succeed message
        |= timestampParser
        |. space
        |. dash
        |. space
        |= userParser
        |. colon
        |. space
        |= contentParser


space : Parser ()
space =
    symbol " "


dash : Parser ()
dash =
    symbol "-"


colon : Parser ()
colon =
    symbol ":"


slash : Parser ()
slash =
    symbol "/"

comma : Parser ()
comma =
    symbol ","


timestampParser : Parser Timestamp
timestampParser =
    succeed timestamp
        |= digits 2
        |. slash
        |= digits 2
        |. slash
        |= digits 4
        |. comma
        |. space
        |= digits 2
        |. colon
        |= digits 2


digits : Int -> Parser Int
digits n =
    seq n Char.isDigit
        |> Parser.map String.fromList
        |> Parser.map String.toInt
        |> Parser.andThen safeIntUnwrap


seq : Int -> (Char -> Bool) -> Parser (List Char)
seq n predicate =
    accumulatedSeq [] n predicate


accumulatedSeq : List Char -> Int -> (Char -> Bool) -> Parser (List Char)
accumulatedSeq accumulator n predicate =
    if n == 0 then
        succeed <| List.reverse accumulator

    else
        let
            headParser =
                getChompedString <|
                    succeed ()
                        |. chompIf predicate

            tailParser input =
                case String.uncons input of
                    Just ( character, _ ) ->
                        accumulatedSeq (character :: accumulator) (n - 1) predicate

                    Nothing ->
                        problem "expected a certain character, but did not see it"
        in
        headParser
            |> Parser.andThen tailParser


safeIntUnwrap : Maybe Int -> Parser Int
safeIntUnwrap option =
    case option of
        Just n ->
            succeed n

        Nothing ->
            problem "can not unwrap Nothing into an Int"


userParser : Parser String
userParser =
    getChompedString <|
        succeed ()
            |. chompUntil ":"


contentParser : Parser String
contentParser =
    getChompedString <|
        succeed ()
            |. chompUntil "\n"
