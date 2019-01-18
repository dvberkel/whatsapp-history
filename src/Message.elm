module Message exposing (Message, message)


type Message
    = Message
        { timestamp : String
        , user : String
        , content : String
        }


message : String -> String -> String -> Message
message timestamp user content =
    Message
        { timestamp = timestamp
        , user = user
        , content = content
        }
