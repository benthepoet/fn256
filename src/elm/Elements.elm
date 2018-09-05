module Elements exposing (column, columns, confirmPassword, email, error, field, label, number, password, spinner, text)

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
        (attributes
            ++ [ Attributes.class "input"
               , Attributes.type_ "password"
               , Attributes.placeholder "Confirm Password"
               ]
        )
        []


email attributes =
    Html.input
        (attributes
            ++ [ Attributes.class "input"
               , Attributes.type_ "email"
               , Attributes.placeholder "Email"
               ]
        )
        []


error message =
    Html.div
        [ Attributes.class "has-text-centered mt-4" ]
        [ Html.h2
            [ Attributes.class "subtitle is-2" ]
            [ Html.text "Error" ]
        , Html.h4
            [ Attributes.class "subtitle is-5" ]
            [ Html.text message ]
        ]


field =
    Html.div
        [ Attributes.class "field" ]


label =
    Html.label
        [ Attributes.class "label is-small" ]


number =
    smallInput "number"

password attributes =
    Html.input
        (attributes
            ++ [ Attributes.class "input"
               , Attributes.type_ "password"
               , Attributes.placeholder "Password"
               ]
        )
        []


smallInput type_ attributes =
    Html.input
        (attributes
            ++ [ Attributes.class "input is-small"
               , Attributes.type_ type_
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


text =
    smallInput "text"
