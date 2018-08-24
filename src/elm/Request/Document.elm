module Request.Document exposing (create, get, list, root, update)

import Data.Document as Document exposing (Document)
import Http
import Json.Decode as Decode
import Request.Api as Api


root =
    "/documents"


create : Maybe String -> Document -> Http.Request Document
create token document =
    let
        request =
            Api.post root token <| Http.expectJson Document.decoder
    in
    Http.request
        { request
            | body = Http.jsonBody <| Document.encoder document
        }


get : Maybe String -> Int -> Http.Request Document
get token id =
    let
        url =
            String.join "/" [ root, String.fromInt id ]

        request =
            Api.get url token <| Http.expectJson Document.decoder
    in
    Http.request request


list : Maybe String -> List ( String, String ) -> Http.Request (List Document)
list token params =
    let
        url =
            root ++ Api.queryString params

        request =
            Api.get url token <| 
                Http.expectJson <|
                    Decode.at [ "data" ] <|
                        Decode.list Document.decoder
    in
    Http.request request


update : Maybe String -> Document -> Http.Request Document
update token document =
    let
        url =
            String.join "/" [ root, String.fromInt document.id ]

        request =
            Api.put url token <| Http.expectJson Document.decoder
    in
    Http.request
        { request
            | body = Http.jsonBody <| Document.encoder document
        }
