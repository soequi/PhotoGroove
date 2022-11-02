module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeString)
import Json.Encode as Encode
import PhotoGroove
import Test exposing (..)


suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))


decoderTest : Test
decoderTest =
    test
        "title defaults to (Untitled)" <|
        \_ ->
            """{"url": "fruits.com", "size": 5}"""
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(Untitled)")
