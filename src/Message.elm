module Message exposing (Message, Sender, Timestamp, message, timestamp, user, system)


type Message
    = Message
        { timestamp : Timestamp
        , sender : Sender
        , content : String
        }


message : Timestamp -> Sender -> String -> Message
message aTimestamp aSender content =
    Message
        { timestamp = aTimestamp
        , sender = aSender
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


type Sender
    = User String
    | System


user : String -> Sender
user username =
    User username

system : Sender
system =
    System
