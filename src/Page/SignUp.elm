module Page.SignUp exposing (Model, Msg(..), init, update, view)

import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request.Auth as Auth
import Route
import View.Icons as Icons


type alias Model =
    { confirmPassword : String
    , email : String
    , isError : Bool
    , isLoading : Bool
    , password : String
    }


type Msg
    = SignUpResponse (Result Http.Error ())
    | Submit
    | TypeConfirmPassword String
    | TypeEmail String
    | TypePassword String


init =
    Model "" "" False False ""


update msg model =
    case msg of
        SignUpResponse (Err _) ->
            ( { model
                | isError = True
                , isLoading = False
              }
            , Cmd.none
            )

        SignUpResponse (Ok ()) ->
            ( { model
                | isError = False
                , isLoading = False
              }
            , Cmd.none
            )

        Submit ->
            ( { model
                | isError = False
                , isLoading = True
              }
            , Http.send SignUpResponse <| Auth.signUp model.email model.password
            )

        TypeConfirmPassword confirmPassword ->
            ( { model | confirmPassword = confirmPassword }
            , Cmd.none
            )

        TypeEmail email ->
            ( { model | email = email }
            , Cmd.none
            )

        TypePassword password ->
            ( { model | password = password }
            , Cmd.none
            )


view model =
    let
        errorView =
            if model.isError then
                Html.article
                    [ Attributes.class "message is-danger has-text-centered" ]
                    [ Html.div
                        [ Attributes.class "message-body p-05" ]
                        [ Html.text "There was a problem with your request." ]
                    ]

            else
                Html.div [] []
    in
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
                        [ Html.text "Sign up for an account" ]
                    , errorView
                    , Html.form
                        [ Events.onSubmit Submit ]
                        [ Elements.field
                            [ Html.p
                                [ Attributes.class "control has-icons-left " ]
                                [ Elements.email
                                    [ Attributes.value model.email
                                    , Events.onInput TypeEmail
                                    ]
                                , Html.span
                                    [ Attributes.class "icon is-small is-left" ]
                                    [ Icons.mail ]
                                ]
                            ]
                        , Elements.field
                            [ Html.p
                                [ Attributes.class "control has-icons-left" ]
                                [ Elements.password
                                    [ Attributes.value model.password
                                    , Events.onInput TypePassword
                                    ]
                                , Html.span
                                    [ Attributes.class "icon is-small is-left" ]
                                    [ Icons.lock ]
                                ]
                            ]
                        , Elements.field
                            [ Html.p
                                [ Attributes.class "control has-icons-left" ]
                                [ Elements.confirmPassword
                                    [ Events.onInput TypeConfirmPassword ]
                                , Html.span
                                    [ Attributes.class "icon is-small is-left" ]
                                    [ Icons.lock ]
                                ]
                            ]
                        , Elements.field
                            [ Html.button
                                [ Attributes.class <|
                                    "button is-link full-width"
                                        ++ (if model.isLoading then
                                                " is-loading"

                                            else
                                                ""
                                           )
                                , Attributes.type_ "submit"
                                , Attributes.disabled <| model.confirmPassword /= model.password
                                ]
                                [ Html.text "Sign Up" ]
                            ]
                        , Html.div
                            [ Attributes.class "content has-text-centered mt-1" ]
                            [ Html.text "Already have an account? "
                            , Html.a
                                [ Route.href <| Route.Public Route.LogIn ]
                                [ Html.text "Log In" ]
                            ]
                        ]
                    ]
                ]
            , Html.div
                [ Attributes.class "has-text-centered content mt-1" ]
                [ Html.a
                    [ Route.href <| Route.Public Route.ResetPassword ]
                    [ Html.text "Forgot your password?" ]
                ]
            ]
        , Elements.column []
        ]
