module History exposing (parse, History, Problem(..))

import Message exposing (Message)

parse : String -> Result Problem History
parse input =
    Err <| General "Implement me"

type History =
    History {
            messages: List Message
        }

type Problem =
    General String
