module Page.NotFound exposing (..)


import Elements
import Html
import Html.Attributes as Attributes


view =
    Html.div
        [ Attributes.class "columns" ]
        [ Elements.column []
        , Elements.column
            [ Html.div 
                [ Attributes.class "has-text-centered mt-4" ]
                [ Html.h2
                    [ Attributes.class "subtitle is-2" ]
                    [ Html.text "Error" ]
                , Html.h4 
                    [ Attributes.class "subtitle is-5" ]
                    [ Html.text "The page you requested was not found." ]
                ]
            ]
        , Elements.column []
        ]