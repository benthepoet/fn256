module Data exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type alias User =
    { email : String
    , token : String
    }


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