module Main exposing (..)

import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Navigation
import Route
import Task


type alias Model =
    { route : Route.Route
    , token : Maybe String
    }


type Msg 
    = LogIn String
    | RouteChange Route.Route


main =
    Navigation.program (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

        
init location =
    let
        route = Route.parse location
    in
        ( Model route Nothing
        , Task.perform RouteChange <| Task.succeed route
        )


subscriptions model =
    Sub.none


update msg model =
    case msg of
        LogIn token ->
            ( { model | token = Just token }
            , Cmd.none
            )
            
        RouteChange route ->
            case route of
                Route.Protected page ->
                    case model.token of
                        Nothing ->
                            ( model, Navigation.modifyUrl (Route.toPath <| Route.Public Route.LogIn) )

                        Just token ->
                            ( { model | route = route }
                            , Cmd.none
                            )

                Route.Public page ->
                    ( { model | route = route }
                    , Cmd.none
                    )


view model =
    let
        elements =
            case model.route of
                Route.Protected Route.Home ->
                    [ Elements.column [] ]
                    
                Route.Public Route.LogIn ->
                    [ Elements.column []
                    , Html.div
                        [ Attributes.class "column is-narrow" ]
                        [ Html.div
                            [ Attributes.class "card login mt-4" ]
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
                                        [ Html.button 
                                            [ Attributes.class "button is-link full-width"
                                            , Attributes.type_ "button"
                                            ]
                                            [ Html.text "Log In" ]  
                                        ]
                                    , Html.div
                                        [ Attributes.class "content has-text-centered mt-1" ]
                                        [ Html.text "Don't have an account? "
                                        , Html.a [] [ Html.text "Sign Up" ]
                                        ]
                                    ]
                                ]
                            ]
                        , Html.div
                            [ Attributes.class "has-text-centered content mt-1" ]
                            [ Html.a 
                                []
                                [ Html.text "Forgot your password?" ]
                            ]
                        ]
                    , Elements.column []
                    ]
                    
                Route.Public Route.NotFound ->
                    [ Elements.column [ Html.text "Not Found" ] ]
                    
                Route.Public Route.SignUp ->
                    [ Elements.column [] ]
    in
        Html.div 
            [ Attributes.class "columns" ] 
            elements