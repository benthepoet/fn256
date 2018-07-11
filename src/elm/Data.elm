module Data exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type alias Document =
    { id: Int
    , name : String
    , owner : Int
    , width : Int
    , height : Int
    }


type alias User =
    { email : String
    , token : String
    }


documentDecoder = 
    Decode.map5 Document
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "owner" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


documentEncoder document =
    Encode.object
        [ ( "id", Encode.int document.id)
        , ( "name", Encode.string document.name )
        , ( "owner", Encode.int document.owner )
        , ( "width", Encode.int document.width )
        , ( "height", Encode.int document.height )
        ]


loginEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


userDecoder =
    Decode.map2 User
        (Decode.field "email" Decode.string)
        (Decode.field "token" Decode.string)
        
        
userEncoder user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        ]