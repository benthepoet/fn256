module Request.Element exposing (..)

import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Http
import Json.Decode as Decode
import Request.Api as Api
import Request.Document


root : Int -> String
root documentId =
    String.join "/" 
        [ Request.Document.root
        , toString documentId
        , "elements"
        ]


create token document element =
    let
        url = root document.id
        request = Api.post url token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Element.encoder element
            , expect = Http.expectJson Element.decoder
            }


list : Maybe String -> Int -> Http.Request (List Element)
list token documentId =
    let
        url = root documentId
        request = Api.get url token
    in
        Http.request
            { request 
            | expect = Http.expectJson 
                <| Decode.at ["data"] 
                <| Decode.list Element.decoder 
            }


update : Maybe String -> Document -> Element -> Http.Request Element
update token document element =
    let
        url = String.join "/" [root document.id, toString element.id]
        request = Api.put url token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Element.encoder element
            , expect = Http.expectJson Element.decoder
            }