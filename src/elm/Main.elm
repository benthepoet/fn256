module Main exposing (..)

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
                    [ Html.div [] [] ]
                    
                Route.Public Route.LogIn ->
                    [ Html.div 
                        [ Attributes.class "col-3" ] 
                        []
                    , Html.div
                        [ Attributes.class "col-3" ]
                        [ Html.div
                            [ Attributes.class "card m-20 p-20" ]
                            [ Html.h2
                                [ Attributes.class "text-center" ]
                                [ Html.small [] [ Html.text "Log in to your account"]
                                ]
                            , Html.form
                                []
                                [ Html.div 
                                    [ Attributes.class "form-group" ]
                                    [ Html.label 
                                        [ Attributes.class "form-label" ]
                                        [ Html.text "Email" ]
                                    , Html.div
                                        [ Attributes.class "input-group" ]
                                        [ Html.span
                                            [ Attributes.class "input-addon" ] 
                                            [ Html.i
                                                [ Attributes.class "petalicon petalicon-user" ]
                                                []
                                            ]
                                        , Html.input
                                            [ Attributes.class "input"
                                            , Attributes.type_ "email"
                                            ]
                                            []
                                        ]
                                    ]
                                , Html.div 
                                    [ Attributes.class "form-group" ]
                                    [ Html.label 
                                        [ Attributes.class "form-label" ]
                                        [ Html.text "Password" ]
                                    , Html.div
                                        [ Attributes.class "input-group" ]
                                        [ Html.span
                                            [ Attributes.class "input-addon" ] 
                                            [ Html.i
                                                [ Attributes.class "petalicon petalicon-lock-locked" ]
                                                []
                                            ]
                                        , Html.input
                                            [ Attributes.class "input"
                                            , Attributes.type_ "password"
                                            ]
                                            []
                                        ]
                                    ]
                                , Html.div 
                                    [ Attributes.class "form-group" ]
                                    [ Html.button 
                                        [ Attributes.class "btn full-width"
                                        , Attributes.type_ "button"
                                        ]
                                        [ Html.text "Log In" ]  
                                    ]
                                , Html.div
                                    [ Attributes.class "form-group text-center" ]
                                    [ Html.text "Don't have an account? "
                                    , Html.a [] [ Html.text "Sign Up" ]
                                    ]
                                ]
                            ]
                        ]
                    , Html.div
                        [ Attributes.class "col-3" ]
                        []
                    ]
                    
                Route.Public Route.NotFound ->
                    [ Html.div [] [ Html.text "Not Found" ] ]
                    
                Route.Public Route.SignUp ->
                    [ Html.div [] [] ]
    in
        Html.div 
            [ Attributes.class "row" ] 
            elements