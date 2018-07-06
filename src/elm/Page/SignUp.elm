module Page.SignUp exposing (..)


import Elements
import Html
import Html.Attributes as Attributes
import Route


view = 
    [ Elements.column []
    , Html.div
        [ Attributes.class "column is-narrow" ]
        [ Html.div
            [ Attributes.class "card w-medium mt-4" ]
            [ Html.div
                [ Attributes.class "card-content" ]
                [ Html.h2
                    [ Attributes.class "subtitle is-4 has-text-centered" ]
                    [ Html.text "Sign up for an account" ]
                , Html.form
                    []
                    [ Elements.field
                        [ Html.p
                            [ Attributes.class "control has-icons-left " ]
                            [ Elements.email []
                            , Html.span
                                [ Attributes.class "icon is-small is-left" ]
                                [ Html.i 
                                    [ Attributes.class "fas fa-envelope" ]
                                    []
                                ]
                            ]
                        ]
                    , Elements.field
                        [ Html.p
                            [ Attributes.class "control has-icons-left" ]
                            [ Elements.password []
                            , Html.span
                                [ Attributes.class "icon is-small is-left" ]
                                [ Html.i 
                                    [ Attributes.class "fas fa-lock" ]
                                    []
                                ]
                            ]
                        ]
                    , Elements.field
                        [ Html.p
                            [ Attributes.class "control has-icons-left" ]
                            [ Elements.confirmPassword []
                            , Html.span
                                [ Attributes.class "icon is-small is-left" ]
                                [ Html.i 
                                    [ Attributes.class "fas fa-lock" ]
                                    []
                                ]
                            ]
                        ]
                    , Elements.field
                        [ Html.button 
                            [ Attributes.class "button is-link full-width"
                            , Attributes.type_ "button"
                            ]
                            [ Html.text "Sign Up" ]  
                        ]
                    , Html.div
                        [ Attributes.class "content has-text-centered mt-1" ]
                        [ Html.text "Already have an account? "
                        , Html.a 
                            [ Attributes.href <| Route.toPath <| Route.Public Route.LogIn ] 
                            [ Html.text "Log In" ]
                        ]
                    ]
                ]
            ]
        , Html.div
            [ Attributes.class "has-text-centered content mt-1" ]
            [ Html.a 
                [ Attributes.href <| Route.toPath <| Route.Public Route.ResetPassword ]
                [ Html.text "Forgot your password?" ]
            ]
        ]
    , Elements.column []
    ]