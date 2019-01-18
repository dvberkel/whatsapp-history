module HistoryTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import History exposing (parse, Problem(..))

suite : Test
suite =
    describe "History"
        [ describe "parse" [
               test "message" <|
                   \_ ->
                   let
                       actual = parse "03/11/2016, 23:08 - Daan van Berkel: Wil jij volgende keer gewoon drinken geven als ik het vraag"

                       expected = Err <| General "Implement me"
                   in
                       Expect.equal actual expected
              ]
        ]
