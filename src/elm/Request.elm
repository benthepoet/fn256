module Request exposing (..)


import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias LoginToken =
    { token : String
    }


api : String -> String
api =
   (++) "/api" 


tokenDecoder =
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
        Http.post url body tokenDecoder
        
signUp email password =
    let
        url = api "/auth/signup"
        body = Http.jsonBody <| loginEncoder email password
    in
        postEmpty url body


postEmpty url body =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }