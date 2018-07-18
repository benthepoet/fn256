module Data.Element exposing (..)


import Json.Decode as Decode


type alias CircleAttributes =
    { x : Int
    , y : Int
    , radius: Int
    }


type alias RectAttributes = 
    { x : Int
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
                <| Decode.map3 CircleAttributes
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.field "radius" Decode.int)
                    
        2 ->
            Decode.map Rect
                <| Decode.map4 RectAttributes
                    (Decode.field "x" Decode.int)
                    (Decode.field "y" Decode.int)
                    (Decode.field "width" Decode.int)
                    (Decode.field "height" Decode.int)
                    
        _ ->
            Decode.fail "The element type is not valid."