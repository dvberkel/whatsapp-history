module HistoryTest exposing (suite)

import Expect exposing (Expectation)
import History exposing (Problem(..), history, parse)
import Message exposing (message, timestamp, user)
import Test exposing (..)


suite : Test
suite =
    describe "History"
        [ describe "parse"
            [ test "message" <|
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
            ]
        ]
