module BreakTest exposing (..)

import Test exposing (..)
import Break exposing (break)
import Expect exposing (Expectation)
{-| Test for a helper function which breaks string by char
-}
suite : Test
suite = 
    describe "Split string once"
            [ test "split string by /" <|
                \_ ->
                    break '/' "some/value"
                        |> Expect.equal ( Ok ("some", "value") )
            , test "split string by multiple /" <|
                \_ ->
                    break '/' "some/value/someother"
                        |> Expect.equal ( Ok ("some", "value/someother") )
            , test "empty string" <|
                \_ ->
                    break '/' ""
                        |> Expect.equal (Err " does not contain /")
            , test "no splitter" <|
                \_ ->
                    break '/' "some.value"
                        |> Expect.equal (Err "some.value does not contain /")
            , test "just splitter" <|
                \_ ->
                    break '/' "/"
                        |> Expect.equal ( Ok ("","") )
            ]