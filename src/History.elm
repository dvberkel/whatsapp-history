module History exposing (History, Problem(..), history, parse)

import Message exposing (Message, message)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Trailing(..), chompIf, chompUntil, getChompedString, run, sequence, succeed, symbol)


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
        |= timestamp
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


timestamp : Parser String
timestamp =
    getChompedString <|
        succeed ()
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf isSlash
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf isSlash
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf isComma
            |. chompIf isSpace
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit
            |. chompIf isColon
            |. chompIf Char.isDigit
            |. chompIf Char.isDigit


isA : Char -> Char -> Bool
isA target character =
    character == target


isSlash : Char -> Bool
isSlash =
    isA '/'


isComma : Char -> Bool
isComma =
    isA ','


isSpace : Char -> Bool
isSpace =
    isA ' '


isColon : Char -> Bool
isColon =
    isA ':'


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
