module Html.Parser exposing (Attribute, Element(..), Tag, attribute, element, elements, isAttribNameChar, nonAttribNameChars, spaceChars, zeroOrMoreHtmlSpaces)

import Parser exposing (..)
import Set exposing (Set, fromList, member, union)


notInSet : Set comparable -> comparable -> Bool
notInSet set c =
    not <| member c set



-- See https://www.w3.org/TR/2011/WD-html5-20110525/common-microsyntaxes.html#space-character


spaceChars : Set Char
spaceChars =
    fromList [ '\t', ' ', '\u{000D}', '\n', '\u{000C}' ]



-- See https://www.w3.org/TR/2011/WD-html5-20110525/syntax.html#attributes-0
-- TODO: Add control characters


nonAttribNameChars : Set Char
nonAttribNameChars =
    union spaceChars <|
        fromList [ '\u{0000}', '"', '\'', '>', '/', '=' ]



-- VSCODE is dumb: "
-- TODO: Add support for character references


unquotedAttribForbiddenChars : Set Char
unquotedAttribForbiddenChars =
    union nonAttribNameChars <|
        fromList [ '<', '`' ]


doubleQuotedAttribForbiddenChars : Set Char
doubleQuotedAttribForbiddenChars =
    fromList [ '"' ]



-- VSCODE is dumb: "


singleQuotedAttribForbiddenChars : Set Char
singleQuotedAttribForbiddenChars =
    fromList [ '\'' ]


isAttribNameChar : Char -> Bool
isAttribNameChar c =
    not <| member c nonAttribNameChars


zeroOrMoreHtmlSpaces : Parser ()
zeroOrMoreHtmlSpaces =
    chompWhile (\c -> member c spaceChars)


oneOrMoreHtmlSpaces : Parser ()
oneOrMoreHtmlSpaces =
    succeed ()
        |. chompIf (\c -> member c spaceChars)
        |. chompWhile (\c -> member c spaceChars)


type Element
    = Void Tag
    | Normal Tag (List Element)
    | Text String


type alias Tag =
    { name : String
    , attributes : List Attribute
    }


type alias Attribute =
    { name : String
    , value : String
    }


elements : Parser (List Element)
elements =
    let
        elementStepper : List Element -> Element -> Step (List Element) (List Element)
        elementStepper revElems elem =
            case elem of
                Text "" ->
                    Done (List.reverse revElems)
                _ ->
                    Loop (elem :: revElems)
        elementsHelp : List Element -> Parser (Step (List Element) (List Element))
        elementsHelp revElems =
            succeed (elementStepper revElems)
                |= element
    in
    loop [] elementsHelp


element : Parser Element
element =
    oneOf
        [ backtrackable voidTag
        , backtrackable normalTag
        , textElement
        ]


partialTag : String -> String -> Parser Tag
partialTag openSymbol closeSymbol =
    succeed Tag
        |. symbol openSymbol
        |. zeroOrMoreHtmlSpaces
        |= getChompedString (chompWhile Char.isAlpha)
        |= oneOf
            [ attributes
            , zeroOrMoreHtmlSpaces |> andThen (\_ -> succeed [])
            ]
        |. zeroOrMoreHtmlSpaces
        |. symbol closeSymbol


voidTag : Parser Element
voidTag =
    Parser.map Void <|
        partialTag "<" "/>"


normalTag : Parser Element
normalTag =
    succeed Normal
        |= partialTag "<" ">"
        |= lazy (\_ -> elements)
        |. partialTag "</" ">"


-- See https://www.w3.org/TR/html51/syntax.html#rawtext-state
-- "<" and EOF are the only characters that aren't valid raw text.
textElement : Parser Element
textElement =
    Parser.map Text <| getChompedString <| chompUntilEndOr "<"


checkTagName : Tag -> Parser ()
checkTagName tag =
    symbol tag.name


attribute : Parser Attribute
attribute =
    let
        attributeName : Parser String
        attributeName =
            getChompedString (chompWhile Char.isAlpha)

        unquotedAttrValue : Parser String
        unquotedAttrValue =
            getChompedString (chompWhile <| notInSet unquotedAttribForbiddenChars)

        doubleQuotedAttrValue : Parser String
        doubleQuotedAttrValue =
            succeed identity
                |. symbol "\""
                |= getChompedString (chompWhile <| notInSet doubleQuotedAttribForbiddenChars)
                |. symbol "\""

        singleQuotedAttrValue : Parser String
        singleQuotedAttrValue =
            succeed identity
                |. symbol "'"
                |= getChompedString (chompWhile <| notInSet singleQuotedAttribForbiddenChars)
                |. symbol "'"
    in
    succeed Attribute
        |= attributeName
        |. zeroOrMoreHtmlSpaces
        |. symbol "="
        |. zeroOrMoreHtmlSpaces
        |= oneOf
            [ doubleQuotedAttrValue
            , singleQuotedAttrValue
            , unquotedAttrValue
            ]


attributes : Parser (List Attribute)
attributes =
    let
        attributesHelp : List Attribute -> Parser (Step (List Attribute) (List Attribute))
        attributesHelp revAttribs =
            oneOf
                [ succeed (\attr -> Loop (attr :: revAttribs))
                    |. backtrackable oneOrMoreHtmlSpaces
                    |= attribute
                , succeed ()
                    |> map (\_ -> Done (List.reverse revAttribs))
                ]
    in
    loop [] attributesHelp



-- sequence
--     { start = ""
--     , separator = " "
--     , end = ">"
--     , spaces = chompWhile (\_ -> False)
--     , item = attribute
--     , trailing = Optional -- demand a trailing semi-colon
--     }
-- attributes : Parser (List Attribute)
-- attributes =
--     sequence
--         { separator = oneOrMoreHtmlSpaces
--         , end = oneOf [ symbol ">", symbol "/>" ]
--         , item = attribute
--         }
-- sequence :
--     { separator : Parser ()
--     , end : Parser ()
--     , item : Parser a
--     }
--     -> Parser (List a)
-- sequence i =
--     sequenceEnd i.end i.item i.separator
-- sequenceEnd : Parser () -> Parser a -> Parser () -> Parser (List a)
-- sequenceEnd ender parseItem sep =
--     let
--         chompRest item =
--             loop [ item ] (sequenceEndOptional ender parseItem sep)
--     in
--     oneOf
--         [ parseItem |> andThen chompRest
--         , ender |> map (\_ -> [])
--         ]
-- sequenceEndOptional :
--     Parser ()
--     -> Parser a
--     -> Parser ()
--     -> List a
--     -> Parser (Step (List a) (List a))
-- sequenceEndOptional ender parseItem sep revItems =
--     let
--         parseEnd =
--             map (\_ -> Done (List.reverse revItems)) ender
--     in
--     oneOf
--         [ skip sep <|
--             oneOf
--                 [ parseItem |> map (\item -> Loop (item :: revItems))
--                 , parseEnd
--                 ]
--         , parseEnd
--         ]
-- skip : Parser ignore -> Parser keep -> Parser keep
-- skip iParser kParser =
--     kParser |. iParser
