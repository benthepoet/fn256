module Request exposing (..)


import Data
import Http
import Json.Decode as Decode


api : String -> String
api =
   (++) "/api" 


queryString : List (String, String) -> String
queryString =
    let
        param (key, value) =
            key ++ "=" ++ value
    in
        (++) "?" << String.join "&" << List.map param


createDocument : Maybe String -> Data.Document -> Http.Request Data.Document
createDocument token document =
    let
        request = post (api "/documents") token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.documentEncoder document
            , expect = Http.expectJson Data.documentDecoder
            }


getDocument : Maybe String -> Int -> Http.Request Data.Document
getDocument token id = 
    let
        request = get (api <| (++) "/documents/" <| toString id) token
    in
        Http.request
            { request | expect = Http.expectJson Data.documentDecoder }


getDocuments : Maybe String -> List (String, String) -> Http.Request (List Data.Document)
getDocuments token params = 
    let
        url = (api "/documents") ++ (queryString params)
        request = get url token
    in 
        Http.request
            { request 
            | expect = Http.expectJson 
                <| Decode.at ["data"] 
                <| Decode.list Data.documentDecoder 
            }


updateDocument : Maybe String -> Data.Document -> Http.Request Data.Document
updateDocument token document =
    let
        request = put (api "/documents/" ++ (toString document.id)) token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.documentEncoder document
            , expect = Http.expectJson Data.documentDecoder
            }


login : String -> String -> Http.Request Data.User
login email password =
    let
        request = post (api "/auth/login") Nothing
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.loginEncoder email password
            , expect = Http.expectJson Data.userDecoder
            }


signUp : String -> String -> Http.Request ()
signUp email password =
    let
        request = post (api "/auth/signup") Nothing
    in
        Http.request
            { request | body = Http.jsonBody <| Data.loginEncoder email password }


make method url token =
    let
        headers = 
            case token of
                Nothing ->
                    []
                    
                Just value ->
                    [ Http.header "Authorization" <| "Bearer " ++ value ]
    in
        { method = method
        , url = url
        , headers = headers
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


get =
    make "GET"


post =
    make "POST"
    
    
put = 
    make "PUT"