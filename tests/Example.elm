module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Parser exposing (..)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "All the parsing"
        [ describe "Tag parsing"
            [ test "Parses a basic tag" <|
                \_ ->
                    run tag "<html>"
                        |> Expect.equal (Ok <| Tag "html" [])
            , test "Parses a tag with leading/trailing whitespace" <|
                \_ ->
                    run tag "< html >"
                        |> Expect.equal (Ok <| Tag "html" [])
            , test "Parses a tag with an attribute" <|
                \_ ->
                    run tag "<html id=\"abc123\">"
                        |> Expect.equal
                            (Ok <|
                                Tag "html"
                                    [ Attribute "id" "abc123" ]
                            )
            , test "Parses a tag with weird spaces" <|
                \_ ->
                    run tag "<html\tid = abc123\tclass = wtf>"
                        |> Expect.equal
                            (Ok <|
                                Tag "html"
                                    [ Attribute "id" "abc123"
                                    , Attribute "class" "wtf"
                                    ]
                            )
            ]
        , describe "Tag Attribute parsing" <|
            [ test "Parses an unquoted attribute" <|
                \_ ->
                    run attribute "foo = bar"
                        |> Expect.equal (Ok <| Attribute "foo" "bar")
            , test "Parses an unquoted attribute with special characters" <|
                \_ ->
                    run attribute "foo=b@r"
                        |> Expect.equal (Ok <| Attribute "foo" "b@r")
            , test "Parses a double-quoted attribute" <|
                \_ ->
                    run attribute "foo = \"bar\""
                        |> Expect.equal (Ok <| Attribute "foo" "bar")
            , test "Parses a double-quoted attribute with special characters" <|
                \_ ->
                    run attribute "foo=\"<'bar'>\""
                        |> Expect.equal (Ok <| Attribute "foo" "<'bar'>")
            , test "Parses a single-quoted attribute" <|
                \_ ->
                    run attribute "foo = 'bar'"
                        |> Expect.equal (Ok <| Attribute "foo" "bar")
            , test "Parses a single-quoted attribute with special characters" <|
                \_ ->
                    run attribute "foo='\"<bar>\"'"
                        |> Expect.equal (Ok <| Attribute "foo" "\"<bar>\"")
            , test "Doesn't parse a '>'" <|
                \_ ->
                    run attribute "foo=bar>"
                        |> Expect.equal (Ok <| Attribute "foo" "bar")
            ]
        ]
