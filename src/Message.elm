module Message exposing (Message)


type Message
    = Message
        { timestamp : String
        , user : String
        , message : String
        }
