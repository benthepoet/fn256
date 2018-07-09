module Page.LogIn exposing (..)


import Debug
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request
import Route


type alias Model =
    { email : String
    , isError : Bool
    , isLoading : Bool
    , password : String
    }


type Msg
    = LoginResponse (Result Http.Error Request.LoginToken)
    | Submit
    | TypeEmail String
    | TypePassword String


type OutMsg
    = NoOp
    | SetToken String
    

init = 
    Model "" False False ""


update msg model =
    case msg of
        LoginResponse (Err _) ->
            ( { model 
                | isError = True
                , isLoading = False }
            , Cmd.none
            , NoOp 
            )
            
        LoginResponse (Ok { token }) ->
            ( { model 
                | isError = False
                , isLoading = False }
            , Cmd.none 
            , SetToken token
            )

        TypeEmail email ->
            ( { model | email = email }
            , Cmd.none
            , NoOp
            )

        TypePassword password ->
            ( { model | password = password }
            , Cmd.none
            , NoOp
            )

        Submit ->
            ( { model 
                | isError = False
                , isLoading = True }
            , Http.send LoginResponse <| Request.login model.email model.password 
            , NoOp
            )


view model = 
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
                    , if model.isError then
                        Html.article
                            [ Attributes.class "message is-danger has-text-centered" ]
                            [ Html.div
                                [ Attributes.class "message-body p-l" ]
                                [ Html.text "There was a problem with your login." ]
                            ]
                      else
                        Html.div [] []
                    , Html.form
                        [ Events.onSubmit Submit ]
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
                            [ Html.p
                                [ Attributes.class "control has-icons-left" ]
                                [ Elements.password
                                    [ Events.onInput TypePassword ]
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
                                [ Attributes.class <| "button is-link full-width" ++ if model.isLoading then " is-loading" else ""
                                , Attributes.type_ "submit"
                                ]
                                [ Html.text "Log In" ]  
                            ]
                        , Html.div
                            [ Attributes.class "content has-text-centered mt-1" ]
                            [ Html.text "Don't have an account? "
                            , Html.a 
                                [ Route.href <| Route.Public Route.SignUp ] 
                                [ Html.text "Sign Up" ]
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