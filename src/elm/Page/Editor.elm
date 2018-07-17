module Page.Editor exposing (..)


import Data
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request
import Svg
import Svg.Attributes


type DocumentState
    = Loading
    | Response (Result Http.Error Data.Document)


type alias Model =
    { document : DocumentState
    }


type Msg
    = LoadDocument (Result Http.Error Data.Document)


init id user = 
    ( Model Loading
    , Http.send LoadDocument <| Request.getDocument (Just user.token) id
    )
    
    
update user msg model =
    let
        token = Maybe.map .token user
    in
        case msg of
            LoadDocument result ->
                ( { model | document = Response result }
                , Cmd.none
                )


view subModel =
    case subModel.document of
        Loading ->
            Html.div
                [ Attributes.class "has-text-centered mt-3" ]
                [ Elements.spinner ]
                
        Response (Err _) ->
            Html.div 
                [ Attributes.class "has-text-centered mt-4" ]
                [ Html.h2
                    [ Attributes.class "subtitle is-2" ]
                    [ Html.text "Error" ]
                , Html.h4 
                    [ Attributes.class "subtitle is-5" ]
                    [ Html.text "The requested document could not be loaded." ]
                ]
                
        Response (Ok document) ->
            let
                width = toString document.width
                height = toString document.height
            in
                Html.div 
                    [ Attributes.class "flex-1 flex-column" ]
                    [ Html.div
                        [ Attributes.class "pl shadow-b has-background-link has-text-white" ]
                        [ Html.span
                            [ Attributes.class "icon pl-1 pr-1" ]
                            [ Elements.fas "file-alt" ]
                        , Html.span
                            [ Attributes.class "has-text-white has-text-semi-bold" ]
                            [ Html.text document.name ]
                        ]
                    , Html.div
                        [ Attributes.class "columns flex-1 mb-0 mt-0" ]
                        [ Html.div
                            [ Attributes.class "column is-narrow pr-0 w-x-small shadow-r has-background-white" ]
                            [ Elements.columns 
                                [ Html.div
                                    [ Attributes.class "column ml-0 mr-0 has-text-centered" ]
                                    [ Html.span
                                        [ Attributes.class "icon tool" ]
                                        [ Elements.fas "mouse-pointer" ]
                                    , Html.hr 
                                        [ Attributes.class "tool" ] 
                                        []
                                    , Html.span
                                        [ Attributes.class "icon tool" ]
                                        [ Elements.fas "barcode" ]
                                    , Html.span
                                        [ Attributes.class "icon tool" ]
                                        [ Elements.fas "font" ]
                                    , Html.span
                                        [ Attributes.class "icon tool" ]
                                        [ Elements.far "square" ]
                                    ]
                                ]
                            ]
                        , Html.div
                            [ Attributes.class "column has-text-centered overflow-y-scroll" ]
                            [ Html.div 
                                [ Attributes.class "mt-1" ]
                                [ Svg.svg 
                                    [ Svg.Attributes.viewBox <| "0 0 " ++ width ++ " " ++ height
                                    , Svg.Attributes.class "shadow"
                                    , Svg.Attributes.style <| "width: " ++ width ++ "px; height: " ++ height ++ "px;"
                                    ]
                                    [ Svg.rect
                                        [ Svg.Attributes.x "0" 
                                        , Svg.Attributes.y "0" 
                                        , Svg.Attributes.width "100%"
                                        , Svg.Attributes.height "100%"
                                        , Svg.Attributes.fill "#fff"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]