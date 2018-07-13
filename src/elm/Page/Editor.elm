module Page.Editor exposing (..)


import Data
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request


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
    , Http.send LoadDocument <| Request.getDocument (Just user.token) <| toString id
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
    Html.div 
        [ Attributes.class "flex-1 flex-column" ]
        [ Html.div
            [ Attributes.class "pl shadow-b has-background-link has-text-white" ]
            [ Html.span
                [ Attributes.class "icon pl-1 pr-1" ]
                [ Elements.fas "file-alt" ]
            , Html.span
                [ Attributes.class "has-text-white has-text-semi-bold" ]
                [ Html.text "My Document" ]
            ]
        , Html.div
            [ Attributes.class "columns flex-1 mb-0 mt-0" ]
            [ Html.div
                [ Attributes.class "column is-narrow w-x-small shadow-r has-background-white" ]
                [ Elements.columns 
                    [ Html.div
                        [ Attributes.class "column ml-0 mr-0 has-text-centered" ]
                        [ Html.span
                            [ Attributes.class "icon tool" ]
                            [ Elements.fas "mouse-pointer" ]
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
                [ Attributes.class "column" ]
                [ Html.div 
                    [ Attributes.class "mt-1" ]
                    []
                ]
            ]
        ]