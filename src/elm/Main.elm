module Main exposing (..)

import Data
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import Page.Editor
import Page.Home
import Page.LogIn
import Page.NotFound
import Page.ResetPassword
import Page.SignUp
import Ports
import Route
import Task


type alias Flags =
    { user : Decode.Value
    }


type alias Model =
    { page : Page
    , user : Maybe Data.User
    }


type Msg 
    = EditorMsg Page.Editor.Model Page.Editor.Msg
    | HomeMsg Page.Home.Model Page.Home.Msg
    | LoginMsg Page.LogIn.Model Page.LogIn.Msg
    | LogOut
    | ResetPasswordMsg Page.ResetPassword.Model Page.ResetPassword.Msg
    | RouteChange Route.Route
    | SignUpMsg Page.SignUp.Model Page.SignUp.Msg


type Page
    = Blank
    | Editor Page.Editor.Model
    | Home Page.Home.Model
    | LogIn Page.LogIn.Model
    | NotFound
    | ResetPassword Page.ResetPassword.Model
    | SignUp Page.SignUp.Model


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
            |> Decode.decodeValue Data.userDecoder 
            |> Result.toMaybe
    in
        ( Model Blank user  
        , Task.perform RouteChange <| Task.succeed route
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg subModel subMsg ->
            let
                ( pageModel, subCmd ) = Page.Editor.update model.user subMsg subModel
            in
                ( { model | page = Editor pageModel }
                , Cmd.map (EditorMsg pageModel) subCmd
                )
    
        HomeMsg subModel subMsg ->
            let
                ( pageModel, subCmd ) = Page.Home.update model.user subMsg subModel
            in
                ( { model | page = Home pageModel }
                , Cmd.map (HomeMsg pageModel) subCmd
                )
    
        LoginMsg subModel subMsg ->
            let
                ( pageModel, subCmd, outCmd ) = Page.LogIn.update subMsg subModel
                ( newModel, cmd ) = 
                    case outCmd of
                        Page.LogIn.NoOp ->
                            ( model, Cmd.none )

                        Page.LogIn.SetUser user ->
                            ( { model | user = Just user }
                            , Cmd.batch 
                                [ Route.navigateTo <| Route.Protected Route.Home
                                , Ports.syncUser <| Data.userEncoder user
                                ]
                            )
            in
                ( { newModel | page = LogIn pageModel }
                , Cmd.batch 
                    [ Cmd.map (LoginMsg pageModel) subCmd
                    , cmd
                    ]
                )
                
        LogOut ->
            ( { model | user = Nothing }
            , Cmd.batch
                [ Route.navigateTo <| Route.Public Route.LogIn
                , Ports.syncUser Encode.null
                ]
            )
            
        ResetPasswordMsg subModel subMsg ->
            ( { model | page = ResetPassword <| Page.ResetPassword.update subMsg subModel }
            , Cmd.none
            )
    
        RouteChange route ->
            case route of
                Route.Protected page ->
                    case model.user of
                        Nothing ->
                            ( model, Route.navigateTo <| Route.Public Route.LogIn )

                        Just user ->
                            case page of
                                Route.Editor id ->
                                    let
                                        ( subModel, subCmd ) = Page.Editor.init id user
                                    in
                                        ( { model | page = Editor subModel }
                                        , Cmd.map (EditorMsg subModel) subCmd
                                        )
                            
                                Route.Home ->
                                    let
                                        ( subModel, subCmd ) = Page.Home.init user
                                    in
                                        ( { model | page = Home subModel }
                                        , Cmd.map (HomeMsg subModel) subCmd
                                        )

                Route.Public page ->
                    case model.user of
                        Nothing ->
                            case page of
                                Route.LogIn ->
                                    ( { model | page = LogIn Page.LogIn.init }
                                    , Cmd.none
                                    )
                                    
                                Route.NotFound ->
                                    ( { model | page = NotFound }
                                    , Cmd.none
                                    )
                                    
                                Route.ResetPassword ->
                                    ( { model | page = ResetPassword Page.ResetPassword.init }
                                    , Cmd.none
                                    )
                                    
                                Route.SignUp ->
                                    ( { model | page = SignUp Page.SignUp.init }
                                    , Cmd.none
                                    )
                                    
                        Just _ ->
                            ( model, Route.navigateTo <| Route.Protected Route.Home )

        SignUpMsg subModel subMsg ->
            let
                ( pageModel, subCmd ) = Page.SignUp.update subMsg subModel
            in
                ( { model | page = SignUp pageModel }
                , Cmd.map (SignUpMsg pageModel) subCmd
                )


frame : Data.User -> Html Msg -> Html Msg
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
                        [ Attributes.class "navbar-item is-active" ]
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


view : Model -> Html Msg
view model =
    case (model.page, model.user) of
        (Editor subModel, Just user) ->
            Page.Editor.view subModel
                |> Html.map (EditorMsg subModel)
                |> frame user
    
        (Home subModel, Just user) ->
            Page.Home.view subModel
                |> Html.map (HomeMsg subModel)
                |> frame user
            
        (LogIn subModel, Nothing) ->
            Page.LogIn.view subModel
                |> Html.map (LoginMsg subModel)  
            
        (NotFound, Nothing) ->
            Page.NotFound.view
            
        (ResetPassword subModel, Nothing) ->
            Page.ResetPassword.view
                |> Html.map (ResetPasswordMsg subModel)
            
        (SignUp subModel, Nothing) ->
            Page.SignUp.view subModel
                |> Html.map (SignUpMsg subModel)
                
        _ ->
            Html.div [] []