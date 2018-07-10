module Request exposing (..)


import Data
import Http
import Json.Decode as Decode


api : String -> String
api =
   (++) "/api" 


createDocument token document =
    let
        request = post (api "/documents") token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.documentEncoder document
            , expect = Http.expectJson Data.documentDecoder
            }


getDocument token id = 
    let
        request = get (api <| "/documents/" ++ id) token
    in
        Http.request
            { request | expect = Http.expectJson Data.documentDecoder }


getDocuments token = 
    let
        request = get (api "/documents") token
    in 
        Http.request
            { request 
            | expect = Http.expectJson 
                <| Decode.at ["data"] 
                <| Decode.list Data.documentDecoder 
            }


updateDocument token document =
    let
        request = put (api "/documents/" ++ (toString document.id)) token
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.documentEncoder document
            , expect = Http.expectJson Data.documentDecoder
            }


login email password =
    let
        request = post (api "/auth/login") Nothing
    in
        Http.request
            { request
            | body = Http.jsonBody <| Data.loginEncoder email password
            , expect = Http.expectJson Data.tokenDecoder
            }


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