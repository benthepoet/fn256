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


field =
    Html.div 
        [ Attributes.class "field" ]


email attributes = 
    Html.input 
        ( attributes ++
            [ Attributes.class "input" 
            , Attributes.type_ "email"
            , Attributes.placeholder "Email"
            ]
        )
        []


password attributes =
    Html.input
        ( attributes ++
            [ Attributes.class "input" 
            , Attributes.type_ "password"
            , Attributes.placeholder "Password"
            ]
        )
        []