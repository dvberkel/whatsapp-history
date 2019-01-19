module History exposing (History, history)

import Message exposing (Message)

type History
    = History
        { messages : List Message
        }


history : List Message -> History
history messages =
    History { messages = messages }
