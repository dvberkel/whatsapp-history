module History.Parser exposing (parse, Problem(..))

import History exposing (History, history)
import Message exposing (Message, Sender, Timestamp, message, system, timestamp, user)
import Parser exposing (..)


parse : String -> Result Problem History
parse input =
    run historyParser input
        |> Result.mapError Parse


type Problem
    = General String
    | Parse (List DeadEnd)


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
        |= senderParser
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


senderParser : Parser Sender
senderParser =
    oneOf
        [ userParser
        , Parser.map (\_ -> system) <| succeed ()
        ]


userParser : Parser Sender
userParser =
    let
        usernameParser =
            getChompedString <|
                succeed ()
                    |. chompUntil ":"
                    |. chompIf isColon
                    |. chompIf isSpace
    in
    usernameParser
        |> Parser.map (String.slice 0 -2)
        |> Parser.map user


isA : Char -> Char -> Bool
isA target character =
    target == character


isColon : Char -> Bool
isColon =
    isA ':'


isSpace : Char -> Bool
isSpace =
    isA ' '


contentParser : Parser String
contentParser =
    getChompedString <|
        succeed ()
            |. chompUntil "\n"
