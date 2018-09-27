module Data.Credential exposing (encoder)

import Json.Encode as Encode


encoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]
