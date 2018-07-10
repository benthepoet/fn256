module Elements exposing (..)


import Html
import Html.Attributes as Attributes


column =
    Html.div
        [ Attributes.class "column" ]


columns =
    Html.div
        [ Attributes.class "columns" ]


confirmPassword attributes =
    Html.input
        ( attributes ++
            [ Attributes.class "input" 
            , Attributes.type_ "password"
            , Attributes.placeholder "Confirm Password"
            ]
        )
        []


email attributes = 
    Html.input 
        ( attributes ++
            [ Attributes.class "input" 
            , Attributes.type_ "email"
            , Attributes.placeholder "Email"
            ]
        )
        []


field =
    Html.div 
        [ Attributes.class "field" ]


password attributes =
    Html.input
        ( attributes ++
            [ Attributes.class "input" 
            , Attributes.type_ "password"
            , Attributes.placeholder "Password"
            ]
        )
        []


spinner =
    Html.span
        [ Attributes.class "icon" ]
        [ Html.i
            [ Attributes.class "fas fa-2x fa-spinner fa-pulse" ]
            []
        ]