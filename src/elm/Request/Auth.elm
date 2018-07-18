module Request.Auth exposing (..)

import Data
import Http
import Request.Api as Api


login : String -> String -> Http.Request Data.User
login email password =
    let
        request = Api.post "/auth/login" Nothing
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.loginEncoder email password
            , expect = Http.expectJson Data.userDecoder
            }


signUp : String -> String -> Http.Request ()
signUp email password =
    let
        request = Api.post "/auth/signup" Nothing
    in
        Http.request
            { request | body = Http.jsonBody <| Data.loginEncoder email password }