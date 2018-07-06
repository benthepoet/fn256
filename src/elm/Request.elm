module Request exposing (..)


import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias LoginToken =
    { token : String
    }


api =
   (++) "/api" 


loginDecoder =
    Decode.map LoginToken <| Decode.field "token" Decode.string


loginEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


login email password =
    let
        url = api "/auth/login"
        body = Http.jsonBody <| loginEncoder email password
    in
        Http.post url body loginDecoder