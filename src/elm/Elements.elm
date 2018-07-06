module Elements exposing (..)


import Html
import Html.Attributes as Attributes


column =
    Html.div
        [ Attributes.class "column" ]


confirmPassword =
    Html.input
        [ Attributes.class "input" 
        , Attributes.type_ "password"
        , Attributes.placeholder "Confirm Password"
        ]


field =
    Html.div 
        [ Attributes.class "field" ]


email = 
    Html.input
        [ Attributes.class "input" 
        , Attributes.type_ "email"
        , Attributes.placeholder "Email"
        ]


password =
    Html.input
        [ Attributes.class "input" 
        , Attributes.type_ "password"
        , Attributes.placeholder "Password"
        ]