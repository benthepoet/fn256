module Page.LogIn exposing (..)


import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Route


type alias Model =
    { email : String
    , password : String
    }


type Msg
    = TypeEmail String
    | TypePassword String


init = 
    Model "" ""


update msg model =
    case msg of
        TypeEmail email ->
            { model | email = email }
            
        TypePassword password ->
            { model | password = password }


view = 
    Html.div
        [ Attributes.class "columns" ]
        [ Elements.column []
        , Html.div
            [ Attributes.class "column is-narrow" ]
            [ Html.div
                [ Attributes.class "card w-medium mt-4" ]
                [ Html.div
                    [ Attributes.class "card-content" ]
                    [ Html.h2
                        [ Attributes.class "subtitle is-4 has-text-centered" ]
                        [ Html.text "Log in to your account" ]
                    , Html.form
                        []
                        [ Elements.field
                            [ Html.p
                                [ Attributes.class "control has-icons-left " ]
                                [ Html.input
                                    [ Attributes.class "input" 
                                    , Attributes.type_ "email"
                                    , Attributes.placeholder "Email"
                                    , Events.onInput TypeEmail
                                    ]
                                    []
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
                                [ Html.input
                                    [ Attributes.class "input" 
                                    , Attributes.type_ "password"
                                    , Attributes.placeholder "Password"
                                    , Events.onInput TypePassword
                                    ]
                                    []
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
                                [ Html.text "Log In" ]  
                            ]
                        , Html.div
                            [ Attributes.class "content has-text-centered mt-1" ]
                            [ Html.text "Don't have an account? "
                            , Html.a 
                                [ Attributes.href <| Route.toPath <| Route.Public Route.SignUp ] 
                                [ Html.text "Sign Up" ]
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