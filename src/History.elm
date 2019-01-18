module History exposing (parse, History, Problem(..))

parse : String -> Result Problem History
parse input =
    Err <| General "Implement me"

type History =
    History {
            messages: List String
        }

type Problem =
    General String
