module Request.Api exposing (..)


import Http
import Json.Decode as Decode


prefix = "https://ek512.benpaulhanna.com"


queryString : List (String, String) -> String
queryString =
    let
        param (key, value) =
            key ++ "=" ++ value
    in
        (++) "?" << String.join "&" << List.map param


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
        , url = prefix ++ url
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