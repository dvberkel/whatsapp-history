module Message exposing (Message, Timestamp, message, timestamp)


type Message
    = Message
        { timestamp : String
        , user : String
        , content : String
        }


message : String -> String -> String -> Message
message aTimestamp user content =
    Message
        { timestamp = aTimestamp
        , user = user
        , content = content
        }


type Timestamp
    = Timestamp
        { dayOfMonth : DayOfMonth
        , monthOfYear : MonthOfYear
        , year : Year
        , hour : Hour
        , minutes : Minutes
        }


type alias DayOfMonth =
    Int


type alias MonthOfYear =
    Int


type alias Year =
    Int


type alias Hour =
    Int


type alias Minutes =
    Int


timestamp : DayOfMonth -> MonthOfYear -> Year -> Hour -> Minutes -> Timestamp
timestamp dayOfMonth monthOfYear year hour minutes =
    Timestamp
        { dayOfMonth = dayOfMonth
        , monthOfYear = monthOfYear
        , year = year
        , hour = hour
        , minutes = minutes
        }
