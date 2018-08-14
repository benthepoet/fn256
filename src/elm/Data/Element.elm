module Data.Element exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type ElementType
    = Circle CircleAttributes
    | Rect RectAttributes
    | TextBox TextBoxAttributes


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
    

type alias TextBoxAttributes =
    { x : Int
    , y : Int
    , text : String
    }


type alias Element =
    { id : Int
    , elementType : ElementType
    }


decodeElementType elementType =
    let
        attributesDecoder =
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
                            
                3 ->
                    Decode.map TextBox
                        <| Decode.map3 TextBoxAttributes
                            (Decode.field "x" Decode.int)
                            (Decode.field "y" Decode.int)
                            (Decode.field "text" Decode.string)
                    
                _ ->
                    Decode.fail "The element type is invalid."
                    
    in
        Decode.map2 Element
            (Decode.field "id" Decode.int)
            (Decode.field "attributes" attributesDecoder)


decoder =
    Decode.field "element_type" Decode.int
        |> Decode.andThen decodeElementType


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
                    
                TextBox attributes ->
                    ( Encode.int 3
                    , Encode.object 
                        [ ("x", Encode.int attributes.x)
                        , ("y", Encode.int attributes.y)
                        , ("text", Encode.string attributes.text)
                        ]
                    )
    in
        Encode.object 
            [ ("element_type", elementType)
            , ("attributes", attributes)
            ]