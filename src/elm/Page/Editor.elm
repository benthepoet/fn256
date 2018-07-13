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
        [ Attributes.class "columns fill-vert" ]
        [ Html.div
            [ Attributes.class "column toolbox is-narrow w-small has-background-white" ]
            []
        , Html.div
            [ Attributes.class "column" ]
            [ Html.div 
                [ Attributes.class "mt-4" ]
                [ Html.text "Document Loaded" ]
            ]
        ]