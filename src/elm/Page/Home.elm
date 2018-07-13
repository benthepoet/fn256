module Page.Home exposing (..)


import Data
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request
import Route


type alias Model =
    { documents : List Data.Document
    , isError : Bool
    , isLoading : Bool
    , selected : Maybe Data.Document
    }


type Msg
    = LoadDocuments (Result Http.Error (List Data.Document))
    | Search String
    | Select Data.Document


init user = 
    ( Model [] False True Nothing
    , Http.send LoadDocuments <| Request.getDocuments (Just user.token) []
    )
    
    
update user msg model =
    let
        token = Maybe.map .token user
    in
        case msg of
            LoadDocuments (Ok documents) ->
                ( { model
                    | documents = documents
                    , isError = False
                    , isLoading = False
                    }
                , Cmd.none
                )
                
            LoadDocuments (Err _) ->
                ({ model
                    | isError = True
                    , isLoading = False
                    }
                , Cmd.none
                )
                
            Search search ->
                ( model
                , Http.send LoadDocuments 
                    <| Request.getDocuments token 
                        [ ("search", search) ]
                )
                
            Select document ->
                ( { model | selected = Just document }
                , Cmd.none
                )


viewDocument selected document =
    let
        isActive =
            case selected of
                Nothing ->
                    False
                
                Just { id } ->
                    id == document.id
    in
        Html.a
            [ Attributes.class <| "panel-block" ++ if isActive then " is-active" else ""
            , Events.onClick <| Select document 
            ]
            [ Html.span
                [ Attributes.class "panel-icon" ]
                [ Html.i 
                    [ Attributes.class "fas fa-file-alt" ]
                    []
                ]
            , Html.text document.name
            ]


viewSelected selected =
    case selected of
        Nothing ->
            Html.div [] []
    
        Just document ->
            Html.div
                [ Attributes.class "card mt-1 mr-1" ]
                [ Html.header
                    [ Attributes.class "card-header" ]
                    [ Html.p 
                        [ Attributes.class "card-header-title" ]
                        [ Html.text document.name ]
                    ]
                , Html.footer
                    [ Attributes.class "card-footer" ]
                    [ Html.a 
                        [ Attributes.class "card-footer-item" 
                        , Route.href <| Route.Protected <| Route.Editor document.id
                        ]
                        [ Html.text "Edit" ]
                    , Html.a 
                        [ Attributes.class "card-footer-item" ]
                        [ Html.text "Delete" ]
                    ]
                ]


view subModel =
    let
        contentView =
            if subModel.isLoading then
                Html.div
                    [ Attributes.class "has-text-centered mt-3" ]
                    [ Elements.spinner ]
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
                                , Events.onInput Search
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
                    ] ++ List.map (viewDocument subModel.selected) subModel.documents 
    in
        Elements.columns
            [ Elements.column [ contentView ]
            , Html.div
                [ Attributes.class "column is-two-thirds" ]
                [ viewSelected subModel.selected ]
            ]