module Request exposing (..)


import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Document =
    { name : String
    , owner : Int
    , width : Int
    , height : Int
    }


type alias LoginToken =
    { token : String
    }


api : String -> String
api =
   (++) "/api" 


documentDecoder = 
    Decode.map4 Document
        (Decode.field "name" Decode.string)
        (Decode.field "owner" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


tokenDecoder =
    Decode.map LoginToken <| Decode.field "token" Decode.string


documentEncoder document =
    Encode.object
        [ ( "name", Encode.string document.name )
        , ( "owner", Encode.int document.owner )
        , ( "width", Encode.int document.width )
        , ( "height", Encode.int document.height )
        ]


loginEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


createDocument token document =
    let
        request = post (api "/documents") token
    in
        Http.request
            { request
            | body = Http.jsonBody <| documentEncoder document
            , expect = Http.expectJson documentDecoder
            }

getDocuments token = 
    let
        request = get (api "/documents") token
    in 
        Http.request
            { request | expect = Http.expectJson documentDecoder }


login email password =
    let
        request = post (api "/auth/login") Nothing
    in
        Http.request
            { request
            | body = Http.jsonBody <| loginEncoder email password
            , expect = Http.expectJson tokenDecoder
            }


signUp email password =
    let
        request = post (api "/auth/signup") Nothing
    in
        Http.request
            { request | body = Http.jsonBody <| loginEncoder email password }


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