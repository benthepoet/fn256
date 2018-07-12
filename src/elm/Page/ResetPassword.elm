module Page.ResetPassword exposing (..)


import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Route


type alias Model =
    { email : String
    }


type Msg
    = TypeEmail String


init = 
    Model ""


update msg model =
    case msg of
        TypeEmail email ->
            { model | email = email }


view = 
    Elements.columns
        [ Elements.column []
        , Html.div
            [ Attributes.class "column is-narrow" ]
            [ Html.div
                [ Attributes.class "card w-medium mt-4" ]
                [ Html.div
                    [ Attributes.class "card-content" ]
                    [ Html.h2
                        [ Attributes.class "subtitle is-4 has-text-centered" ]
                        [ Html.text "Reset your password" ]
                    , Html.form
                        []
                        [ Elements.field
                            [ Html.p
                                [ Attributes.class "control has-icons-left " ]
                                [ Elements.email 
                                    [ Events.onInput TypeEmail ]
                                , Html.span
                                    [ Attributes.class "icon is-small is-left" ]
                                    [ Html.i 
                                        [ Attributes.class "fas fa-envelope" ]
                                        []
                                    ]
                                ]
                            ]
                        , Elements.field
                            [ Html.button 
                                [ Attributes.class "button is-link full-width"
                                , Attributes.type_ "button"
                                ]
                                [ Html.text "Reset Password" ]  
                            ]
                        , Html.div
                            [ Attributes.class "content has-text-centered mt-1" ]
                            [ Html.a 
                                [ Route.href <| Route.Public Route.LogIn ] 
                                [ Html.text "Log In" ]
                            , Html.text " or "
                            , Html.a 
                                [ Route.href <| Route.Public Route.SignUp ] 
                                [ Html.text "Sign Up" ]
                            ]
                        ]
                    ]
                ]
            ]
        , Elements.column []
        ]