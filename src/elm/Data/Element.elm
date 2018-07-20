module Data.Element exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type ElementType
    = Circle
    | Rect


type alias Element =
    { id : Int
    , elementType : ElementType
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , radius : Int
    }


decoder = 
    Decode.map7 Element
        (Decode.field "id" Decode.int)
        (Decode.field "element_type" decodeElementType)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "radius" Decode.int)



decodeElementType =
    Decode.int
        |> Decode.andThen (\elementType -> 
            case elementType of
                1 ->
                    Decode.succeed Circle
                    
                2 ->
                    Decode.succeed Rect
                    
                _ ->
                    Decode.fail "The element type is invalid."
        )


encoder element =
    Encode.object
        [ ("x", Encode.int element.x)
        , ("y", Encode.int element.y)
        , ("width", Encode.int element.width)
        , ("height", Encode.int element.height)
        , ("radius", Encode.int element.radius)
        ]