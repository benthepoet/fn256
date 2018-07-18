module Request.Auth exposing (..)

import Data.Credential as Credential
import Data.User as User exposing (User)
import Http
import Json.Encode as Encode
import Request.Api as Api


login : String -> String -> Http.Request User
login email password =
    let
        request = Api.post "/auth/login" Nothing
    in
        Http.request
            { request
            | body = Http.jsonBody <| Credential.encoder email password
            , expect = Http.expectJson User.decoder
            }


signUp : String -> String -> Http.Request ()
signUp email password =
    let
        request = Api.post "/auth/signup" Nothing
    in
        Http.request
            { request 
            | body = Http.jsonBody <| Credential.encoder email password 
            }