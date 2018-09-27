module Request.Auth exposing (login, root, signUp)

import Data.Credential as Credential
import Data.User as User exposing (User)
import Http
import Json.Encode as Encode
import Request.Api as Api


root =
    "/auth"


login : String -> String -> Http.Request User
login email password =
    let
        url =
            String.join "/" [ root, "login" ]

        request =
            Api.post url Nothing <| Http.expectJson User.decoder
    in
    Http.request
        { request
            | body = Http.jsonBody <| Credential.encoder email password
        }


signUp : String -> String -> Http.Request ()
signUp email password =
    let
        url =
            String.join "/" [ root, "signup" ]

        request =
            Api.post url Nothing <| Http.expectStringResponse (\_ -> Ok ())
    in
    Http.request
        { request
            | body = Http.jsonBody <| Credential.encoder email password
        }
