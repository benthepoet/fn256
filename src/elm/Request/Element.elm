module Request.Element exposing (create, list, root, update)

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
        , String.fromInt documentId
        , "elements"
        ]


create : Maybe String -> Document -> Element.Input -> Http.Request Element
create token document element =
    let
        url =
            root document.id

        request =
            Api.post url token <| Http.expectJson Element.decoder
    in
    Http.request
        { request
            | body = Http.jsonBody <| Element.encoder element
        }


list : Maybe String -> Int -> Http.Request (List Element)
list token documentId =
    let
        url =
            root documentId

        request =
            Api.get url token <|
                Http.expectJson <|
                    Decode.at [ "data" ] <|
                        Decode.list Element.decoder
    in
    Http.request request


update : Maybe String -> Document -> Element -> Http.Request Element
update token document element =
    let
        url =
            String.join "/" [ root document.id, String.fromInt element.id ]

        request =
            Api.put url token <| Http.expectJson Element.decoder
    in
    Http.request
        { request
            | body = Http.jsonBody <| Element.encoder element
        }
