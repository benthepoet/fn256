module Page.Editor.Properties exposing (height, radius, strokeWidth, text, width, x, y)

import Elements
import Events.Html
import Html
import Html.Attributes as Attributes
import Html.Events as Events


height element msg =
    Elements.field
        [ Elements.label [ Html.text "Height" ]
        , Elements.number
            [ Attributes.value <| String.fromInt element.height
            , Events.Html.onInputInt msg
            ]
        ]


radius element msg =
    Elements.field
        [ Elements.label [ Html.text "Radius" ]
        , Elements.number
            [ Attributes.value <| String.fromInt element.radius 
            , Events.Html.onInputInt msg
            ]
        ]


strokeWidth element msg =
    Elements.field
        [ Elements.label [ Html.text "Stroke Width" ]
        , Elements.number
            [ Attributes.value <| String.fromInt element.strokeWidth
            , Attributes.min "0"
            , Events.Html.onInputInt msg
            ]
        ]


text element msg =
    Elements.field
        [ Elements.label [ Html.text "Text" ]
        , Elements.text
            [ Attributes.value element.text 
            , Events.onInput msg
            ]
        ]


width element msg = 
    Elements.field
        [ Elements.label [ Html.text "Width" ]
        , Elements.number
            [ Attributes.value <| String.fromInt element.width
            , Events.Html.onInputInt msg 
            ]
        ]


x element = 
    Elements.field
        [ Elements.label [ Html.text "X" ]
        , Elements.text
            [ Attributes.value <| String.fromInt element.x ]
        ]


y element =
    Elements.field
        [ Elements.label [ Html.text "Y" ]
        , Elements.text
            [ Attributes.value <| String.fromInt element.y ]
        ]