module Data.Element exposing (Element, ElementType(..), Input, circle, decoder, encoder, rect, textBox)

import Json.Decode as Decode
import Json.Encode as Encode


type ElementType
    = Circle
    | Rect
    | TextBox


type alias Element =
    { id : Int
    , elementType : ElementType
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , radius : Int
    , text : String
    }


type alias Input =
    { elementType : ElementType
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , radius : Int
    , text : String
    }


build : ElementType -> Input
build elementType =
    { elementType = elementType
    , x = 0
    , y = 0
    , width = 0
    , height = 0
    , radius = 0
    , text = ""
    }


circle : Input
circle =
    build Circle


rect : Input
rect =
    build Rect


textBox : Input
textBox =
    build TextBox


decodeElementType elementType =
    case elementType of
        1 ->
            Decode.succeed Circle

        2 ->
            Decode.succeed Rect

        3 ->
            Decode.succeed TextBox

        _ ->
            Decode.fail "The element type is invalid."


decoder =
    Decode.map8 Element
        (Decode.field "id" Decode.int)
        (Decode.field "element_type" Decode.int |> Decode.andThen decodeElementType)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "radius" Decode.int)
        (Decode.field "text" Decode.string)


encodeElementType elementType =
    case elementType of
        Circle ->
            Encode.int 1

        Rect ->
            Encode.int 2

        TextBox ->
            Encode.int 3


encoder element =
    Encode.object
        [ ( "element_type", encodeElementType element.elementType )
        , ( "x", Encode.int element.x )
        , ( "y", Encode.int element.y )
        , ( "width", Encode.int element.width )
        , ( "height", Encode.int element.height )
        , ( "radius", Encode.int element.radius )
        , ( "text", Encode.string element.text )
        ]
