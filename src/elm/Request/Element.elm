module Request.Element exposing (..)

import Data.Element as Element exposing (Element)
import Http
import Json.Decode as Decode
import Request.Api as Api
import Request.Document


root documentId =
    String.join "/" [Request.Document.root, documentId, "elements"]


list token documentId =
    let
        url = root <| toString documentId
        request = Api.get url token
    in
        Http.request
            { request 
            | expect = Http.expectJson 
                <| Decode.at ["data"] 
                <| Decode.list Element.decoder 
            }