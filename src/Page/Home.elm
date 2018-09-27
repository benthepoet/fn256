module Page.Home exposing (Model, Msg(..), init, isDocumentActive, update, view, viewDocument, viewSelected)

import Data.Document exposing (Document)
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request.Document
import Route
import View.Icons as Icons


type alias Model =
    { documents : List Document
    , isError : Bool
    , isLoading : Bool
    , selected : Maybe Document
    }


type Msg
    = LoadDocuments (Result Http.Error (List Document))
    | Search String
    | Select Document


init user =
    ( Model [] False True Nothing
    , Http.send LoadDocuments <| Request.Document.list (Just user.token) []
    )


isDocumentActive document =
    Maybe.map (.id >> (==) document.id)
        >> Maybe.withDefault False


update user msg model =
    let
        token =
            Maybe.map .token user
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
            ( { model
                | isError = True
                , isLoading = False
              }
            , Cmd.none
            )

        Search search ->
            ( model
            , Http.send LoadDocuments <|
                Request.Document.list token
                    [ ( "search", search ) ]
            )

        Select document ->
            ( { model | selected = Just document }
            , Cmd.none
            )


viewDocument selected document =
    Html.a
        [ Attributes.classList
            [ ( "panel-block", True )
            , ( "is-active", isDocumentActive document selected )
            ]
        , Events.onClick <| Select document
        ]
        [ Html.span
            [ Attributes.class "panel-icon" ]
            [ Icons.file ]
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
                <|
                    [ Html.p
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
                                [ Icons.search ]
                            ]
                        ]
                    ]
                        ++ List.map (viewDocument subModel.selected) subModel.documents
    in
    Elements.columns
        [ Elements.column [ contentView ]
        , Html.div
            [ Attributes.class "column is-two-thirds" ]
            [ viewSelected subModel.selected ]
        ]
