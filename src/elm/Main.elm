module Main exposing (..)

import Data.User exposing (User)
import Elements
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import Page.Editor as Editor
import Page.Home as Home
import Page.LogIn as LogIn
import Page.NotFound as NotFound
import Page.ResetPassword as ResetPassword
import Page.SignUp as SignUp
import Ports
import Route
import Task


type alias Flags =
    { user : Decode.Value
    }


type alias Model =
    { page : PageState
    , user : Maybe User
    }


type Msg 
    = EditorLoaded (Result Http.Error Editor.Model)
    | EditorMsg Editor.Msg
    | HomeMsg Home.Msg
    | LogInMsg LogIn.Msg
    | LogOut
    | ResetPasswordMsg ResetPassword.Msg
    | RouteChange Route.Route
    | SignUpMsg SignUp.Msg


type Page
    = Editor Editor.Model
    | Home Home.Model
    | LogIn LogIn.Model
    | NotFound
    | ResetPassword ResetPassword.Model
    | SignUp SignUp.Model


type PageState
    = Loading
    | Loaded Page


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (RouteChange << Route.parse)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> Navigation.Location -> (Model, Cmd Msg)
init flags location =
    let
        route = Route.parse location
        user = flags.user
            |> Decode.decodeValue Data.User.decoder 
            |> Result.toMaybe
    in
        ( Model Loading user  
        , Task.perform RouteChange <| Task.succeed route
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.documentPosition <| EditorMsg << Editor.DocumentPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        (EditorLoaded (Err err), _) ->
            let
                a = Debug.log "err" err
            in
                ( model, Cmd.none )
                
        (EditorLoaded (Ok subModel), _) ->
            ( { model | page = Loaded <| Editor subModel }
            , Cmd.none
            )
    
        (EditorMsg subMsg, Loaded (Editor subModel)) ->
            let
                ( pageModel, subCmd ) = Editor.update model.user subMsg subModel
            in
                ( { model | page = Loaded <| Editor pageModel }
                , Cmd.map EditorMsg subCmd
                )
        
        (HomeMsg subMsg, Loaded (Home subModel)) ->
            let
                ( pageModel, subCmd ) = Home.update model.user subMsg subModel
            in
                ( { model | page = Loaded <| Home pageModel }
                , Cmd.map HomeMsg subCmd
                )
    
        (LogInMsg subMsg, Loaded (LogIn subModel)) ->
            let
                ( pageModel, subCmd, outCmd ) = LogIn.update subMsg subModel
                ( newModel, cmd ) = 
                    case outCmd of
                        LogIn.NoOp ->
                            ( model, Cmd.none )

                        LogIn.SetUser user ->
                            ( { model | user = Just user }
                            , Cmd.batch 
                                [ Route.navigateTo <| Route.Protected Route.Home
                                , Ports.syncUser <| Data.User.encoder user
                                ]
                            )
            in
                ( { newModel | page = Loaded <| LogIn pageModel }
                , Cmd.batch 
                    [ Cmd.map LogInMsg subCmd
                    , cmd
                    ]
                )
                
        (LogOut, _) ->
            ( { model | user = Nothing }
            , Cmd.batch
                [ Route.navigateTo <| Route.Public Route.LogIn
                , Ports.syncUser Encode.null
                ]
            )
            
        (ResetPasswordMsg subMsg, Loaded (ResetPassword subModel)) ->
            ( { model | page = Loaded <| ResetPassword <| ResetPassword.update subMsg subModel }
            , Cmd.none
            )
    
        (RouteChange route, _) ->
            case route of
                Route.Protected page ->
                    case model.user of
                        Nothing ->
                            ( model, Route.navigateTo <| Route.Public Route.LogIn )

                        Just user ->
                            case page of
                                Route.Editor id ->
                                    let
                                        task = Editor.init id user
                                    in
                                        ( { model | page = Loading }
                                        , Task.attempt EditorLoaded task
                                        )
                            
                                Route.Home ->
                                    let
                                        ( subModel, subCmd ) = Home.init user
                                    in
                                        ( { model | page = Loaded <| Home subModel }
                                        , Cmd.map HomeMsg subCmd
                                        )

                Route.Public page ->
                    case model.user of
                        Nothing ->
                            case page of
                                Route.LogIn ->
                                    ( { model | page = Loaded <| LogIn LogIn.init }
                                    , Cmd.none
                                    )
                                    
                                Route.NotFound ->
                                    ( { model | page = Loaded <| NotFound }
                                    , Cmd.none
                                    )
                                    
                                Route.ResetPassword ->
                                    ( { model | page = Loaded <| ResetPassword ResetPassword.init }
                                    , Cmd.none
                                    )
                                    
                                Route.SignUp ->
                                    ( { model | page = Loaded <| SignUp SignUp.init }
                                    , Cmd.none
                                    )
                                    
                        Just _ ->
                            ( model, Route.navigateTo <| Route.Protected Route.Home )

        (SignUpMsg subMsg, Loaded (SignUp subModel)) ->
            let
                ( pageModel, subCmd ) = SignUp.update subMsg subModel
            in
                ( { model | page = Loaded <| SignUp pageModel }
                , Cmd.map SignUpMsg subCmd
                )

        _ ->
            ( model, Cmd.none )


frame : User -> Html Msg -> Html Msg
frame user pageView = 
    Html.div
        [ Attributes.class "flex-column h-100-vh" ]
        [ Html.nav
            [ Attributes.class "navbar is-dark" ]
            [ Html.div
                [ Attributes.class "navbar-brand" ]
                [ Html.a 
                    [ Attributes.class "navbar-item" ]
                    [ Html.h4 
                        [ Attributes.class "subtitle is-4 has-text-white" ]
                        [ Html.text "FN256" ] 
                    ]
                ]
            , Html.div
                [ Attributes.class "navbar-menu" ]
                [ Html.div
                    [ Attributes.class "navbar-start" ]
                    [ Html.a 
                        [ Attributes.class "navbar-item is-active" 
                        , Route.href <| Route.Protected Route.Home
                        ]
                        [ Html.text "Home" ]
                    ]
                ]
            , Html.div 
                [ Attributes.class "navbar-end" ]
                [ Html.div
                    [ Attributes.class "navbar-item has-dropdown is-hoverable" ]
                    [ Html.a
                        [ Attributes.class "navbar-link" ]
                        [ Html.text user.email ]
                    , Html.div
                        [ Attributes.class "navbar-dropdown" ]
                        [ Html.a
                            [ Attributes.class "navbar-item" 
                            , Events.onClick LogOut
                            ]
                            [ Html.text "Log Out"]
                        ]
                    ]
                ]
            ]
        , pageView
        ]


viewLoading : Html Msg
viewLoading =
    Html.div
        [ Attributes.class "has-text-centered mt-3" ]
        [ Elements.spinner ]


view : Model -> Html Msg
view model =
    case (model.page, model.user) of
        (Loaded (Editor subModel), Just user) ->
            Editor.view subModel
                |> Html.map EditorMsg
                |> frame user
    
        (Loaded (Home subModel), Just user) ->
            Home.view subModel
                |> Html.map HomeMsg
                |> frame user
            
        (Loaded (LogIn subModel), Nothing) ->
            LogIn.view subModel
                |> Html.map LogInMsg  
            
        (Loaded NotFound, Nothing) ->
            NotFound.view
            
        (Loaded (ResetPassword subModel), Nothing) ->
            ResetPassword.view
                |> Html.map ResetPasswordMsg
            
        (Loaded (SignUp subModel), Nothing) ->
            SignUp.view subModel
                |> Html.map SignUpMsg
            
        (Loading, Just user) ->
            viewLoading
                |> frame user
                
        _ ->
            viewLoading