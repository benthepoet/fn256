module Data.Element exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type ElementType
    = Circle CircleAttributes
    | Rect RectAttributes


type alias CircleAttributes =
    { x : Int
    , y : Int
    , radius : Int
    }
    

type alias RectAttributes =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Element =
    { id : Int
    , elementType : ElementType
    }


decoder = 
    Decode.map2 Element
        (Decode.field "id" Decode.int)
        (decodeElementType)


decodeElementType =
    Decode.field "element_type" Decode.int
        |> Decode.andThen (\elementType -> 
            Decode.field "attributes"
                <| case elementType of
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
                        Decode.fail "The element type is invalid."
        )


encoder element =
    let 
        ( elementType, attributes ) =
            case element.elementType of
                Circle attributes ->
                    ( Encode.int 1
                    , Encode.object 
                        [ ("x", Encode.int attributes.x)
                        , ("y", Encode.int attributes.y)
                        , ("radius", Encode.int attributes.radius)
                        ]
                    )
                    
                Rect attributes ->
                    ( Encode.int 2
                    , Encode.object 
                        [ ("x", Encode.int attributes.x)
                        , ("y", Encode.int attributes.y)
                        , ("width", Encode.int attributes.width)
                        , ("height", Encode.int attributes.height)
                        ]
                    )
    in
        Encode.object 
            [ ("element_type", elementType)
            , ("attributes", attributes)
            ]