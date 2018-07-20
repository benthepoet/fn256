module Request.Element exposing (..)

import Data.Document exposing (Document)
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


update : Maybe String -> Document -> Element -> Http.Request Element
update token document element =
    let
        elementId =
            case element of
                Element.Circle { id } ->
                    id
                    
                Element.Rect { id } ->
                    id

        url = String.join "/" [root <| toString document.id, toString elementId]
        request = Api.put url token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Element.encoder element
            , expect = Http.expectJson Element.decoder
            }