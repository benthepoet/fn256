module Data.Element exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type alias CircleAttributes =
    { id : Int
    , x : Int
    , y : Int
    , radius: Int
    }


type alias RectAttributes = 
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type Element 
    = Circle CircleAttributes
    | Rect RectAttributes
    
    
decoder = 
    Decode.field "element_type" Decode.int
        |> Decode.andThen decodeElement


decodeElement elementType =
    case elementType of
        1 ->
            Decode.map Circle 
                <| Decode.map4 CircleAttributes
                    (Decode.field "id" Decode.int)
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.field "radius" Decode.int)
                    
        2 ->
            Decode.map Rect
                <| Decode.map5 RectAttributes
                    (Decode.field "id" Decode.int)
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.field "width" Decode.int)
                    (Decode.field "height" Decode.int)
                    
        _ ->
            Decode.fail "The element type is not valid."


encoder element =
    case element of
        Circle attributes ->
            Encode.object
                [ ("x", Encode.int attributes.x)
                , ("y", Encode.int attributes.y)
                , ("radius", Encode.int attributes.radius)
                ]
                
        Rect attributes ->
            Encode.object
                [ ("x", Encode.int attributes.x)
                , ("y", Encode.int attributes.y)
                , ("width", Encode.int attributes.width)
                , ("height", Encode.int attributes.height)
                ]