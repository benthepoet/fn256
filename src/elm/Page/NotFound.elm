module Page.NotFound exposing (..)


import Elements
import Html
import Html.Attributes as Attributes


view =
    Html.div
        [ Attributes.class "columns" ]
        [ Elements.column
            [ Html.text "The page could not be found." ]
        ]