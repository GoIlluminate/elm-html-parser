module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Parser exposing (..)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "All the parsing"
        [ describe "Void tag parsing"
            [ test "Parses a basic tag" <|
                \_ ->
                    run element "<html/>"
                        |> Expect.equal (Ok <| Void <| Tag "html" [])
            , test "Parses a tag with leading/trailing whitespace" <|
                \_ ->
                    run element "< html />"
                        |> Expect.equal (Ok <| Void <| Tag "html" [])
            , test "Parses a tag with an attribute" <|
                \_ ->
                    run element "<html id=\"abc123\"/>"
                        |> Expect.equal
                            (Ok <|
                                Void <|
                                    Tag "html"
                                        [ Attribute "id" "abc123" ]
                            )
            , test "Parses a tag with weird spaces" <|
                \_ ->
                    run element "<html\tid = abc123\tclass = wtf/>"
                        |> Expect.equal
                            (Ok <|
                                Void <|
                                    Tag "html"
                                        [ Attribute "id" "abc123"
                                        , Attribute "class" "wtf"
                                        ]
                            )
            ]
        , describe "Normal tag parsing"
            [ test "Parses an empty tag" <|
                \_ ->
                    run element "<html></html>"
                        |> Expect.equal (Ok <| Normal (Tag "html" []) [])
            , test "Parses a tag with inner text" <|
                \_ ->
                    run element "<b>this is bold</b>"
                        |> Expect.equal (Ok <| Normal (Tag "b" []) [ Text "this is bold" ])
            ]
        , describe "Text tag parsing"
            [ test "Parses normal text" <|
                \_ ->
                    run element "this is some text"
                        |> Expect.equal (Ok <| Text "this is some text")
            , test "Parses text with special characters" <|
                \_ ->
                    run element "this > is \\ some / wierd = \"text\""
                        |> Expect.equal (Ok <| Text "this > is \\ some / wierd = \"text\"")
            ]
        , describe "Parsing multiple elements"
            [ test "Doesn't parse an empty text" <|
                \_ ->
                    run elements ""
                        |> Expect.equal (Ok [])
            , test "Parses multiple elements" <|
                \_ ->
                    run elements "<head>asdf</head><body>qwerty</body>"
                        |> Expect.equal
                            (Ok
                                [ Normal (Tag "head" []) [ Text "asdf" ]
                                , Normal (Tag "body" []) [ Text "qwerty" ]
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
