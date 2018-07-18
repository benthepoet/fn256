module Data.Document exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type alias Document =
    { id: Int
    , name : String
    , owner : Int
    , width : Int
    , height : Int
    }


decoder = 
    Decode.map5 Document
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "owner" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


encoder document =
    Encode.object
        [ ( "id", Encode.int document.id)
        , ( "name", Encode.string document.name )
        , ( "owner", Encode.int document.owner )
        , ( "width", Encode.int document.width )
        , ( "height", Encode.int document.height )
        ]