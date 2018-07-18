module Data.User exposing (..)


import Json.Decode as Decode
import Json.Encode as Encode


type alias User =
    { email : String
    , token : String
    }


decoder =
    Decode.map2 User
        (Decode.field "email" Decode.string)
        (Decode.field "token" Decode.string)
        
        
encoder user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        ]