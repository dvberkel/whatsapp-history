module HistoryTest exposing (suite)

import Expect exposing (Expectation)
import History exposing (Problem(..), history, parse)
import Message exposing (message, timestamp, user, system)
import Test exposing (..)


suite : Test
suite =
    describe "History"
        [ describe "parse"
            [ test "user message" <|
                \_ ->
                    let
                        actual =
                            parse "03/11/2016, 23:08 - Daan van Berkel: Test\n"

                        aTimestamp =
                            timestamp 3 11 2016 23 8

                        aSender =
                            user "Daan van Berkel"

                        msg =
                            message aTimestamp aSender "Test"

                        expected =
                            Ok <| history [ msg ]
                    in
                    Expect.equal actual expected
            ,  test "system message" <|
                \_ ->
                    let
                        actual =
                            parse "03/11/2016, 23:08 - Messages to this chat and calls are now secured with end-to-end encryption. Tap for more info.\n"

                        aTimestamp =
                            timestamp 3 11 2016 23 8

                        aSender =
                            system

                        msg =
                            message aTimestamp aSender "Messages to this chat and calls are now secured with end-to-end encryption. Tap for more info."

                        expected =
                            Ok <| history [ msg ]
                    in
                    Expect.equal actual expected

            ]
        ]
