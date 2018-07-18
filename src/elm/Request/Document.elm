module Request.Document exposing (..)


import Data.Document as Document exposing (Document)
import Http
import Json.Decode as Decode
import Request.Api as Api


root = "/documents"


create : Maybe String -> Document -> Http.Request Document
create token document =
    let
        request = Api.post root token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Document.encoder document
            , expect = Http.expectJson Document.decoder
            }


get : Maybe String -> Int -> Http.Request Document
get token id = 
    let
        request = Api.get (String.join "/" [root, toString id]) token
    in
        Http.request
            { request | expect = Http.expectJson Document.decoder }


list : Maybe String -> List (String, String) -> Http.Request (List Document)
list token params = 
    let
        url = root ++ (Api.queryString params)
        request = Api.get url token
    in 
        Http.request
            { request 
            | expect = Http.expectJson 
                <| Decode.at ["data"] 
                <| Decode.list Document.decoder 
            }


update : Maybe String -> Document -> Http.Request Document
update token document =
    let
        request = Api.put (String.join "/" [root, toString document.id]) token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Document.encoder document
            , expect = Http.expectJson Document.decoder
            }