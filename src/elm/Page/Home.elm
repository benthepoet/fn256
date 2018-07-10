module Page.Home exposing (..)


import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request


type alias Model =
    { documents : List Request.Document
    , isError : Bool
    , isLoading : Bool
    }


type Msg
    = LoadDocuments (Result Http.Error (List Request.Document))


init token = 
    ( Model [] False True
    , Http.send LoadDocuments <| Request.getDocuments <| Just token
    )
    
    
update msg model =
    case msg of
        LoadDocuments (Ok documents) ->
            { model
            | documents = documents
            , isError = False
            , isLoading = False
            }
            
        LoadDocuments (Err _) ->
            { model
            | isError = True
            , isLoading = False
            }


viewDocument document =
    Html.a
        [ Attributes.class "panel-block" ]
        [ Html.span
            [ Attributes.class "panel-icon" ]
            [ Html.i 
                [ Attributes.class "fas fa-file-alt" ]
                []
            ]
        , Html.text document.name
        ]


view subModel =
    let
        content =
            if subModel.isLoading then
                Html.div
                    [ Attributes.class "has-text-centered mt-3" ]
                    [ Html.span
                        [ Attributes.class "icon" ]
                        [ Html.i
                            [ Attributes.class "fas fa-2x fa-spinner fa-pulse" ]
                            []
                        ]
                    ]
            else
                Html.nav
                    [ Attributes.class "panel mt-1 ml-1 has-background-white" ]
                    <| [ Html.p
                        [ Attributes.class "panel-heading" ]
                        [ Html.text "My Documents" ]
                    , Html.div
                        [ Attributes.class "panel-block" ]
                        [ Html.p
                            [ Attributes.class "control has-icons-left" ]
                            [ Html.input
                                [ Attributes.class "input"
                                , Attributes.placeholder "Search"
                                , Attributes.type_ "text"
                                ]
                                []
                            , Html.span
                                [ Attributes.class "icon is-left" ]
                                [ Html.i 
                                    [ Attributes.class "fas fa-search" ]
                                    []
                                ]
                            ]
                        ]
                    ] ++ List.map viewDocument subModel.documents 
    in
        Elements.columns
            [ Elements.column 
                [ content ]
            , Elements.column []
            , Elements.column []
            ]