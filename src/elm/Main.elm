module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Interop
import Navigation
import Page.Home
import Page.LogIn
import Page.NotFound
import Page.ResetPassword
import Page.SignUp
import Route
import Task


type alias Flags =
    { token : Maybe String
    }


type alias Model =
    { page : Page
    , token : Maybe String
    }


type Msg 
    = HomeMsg Page.Home.Model Page.Home.Msg
    | LoginMsg Page.LogIn.Model Page.LogIn.Msg
    | LogOut
    | ResetPasswordMsg Page.ResetPassword.Model Page.ResetPassword.Msg
    | RouteChange Route.Route
    | SignUpMsg Page.SignUp.Model Page.SignUp.Msg


type Page
    = Blank
    | Home Page.Home.Model
    | LogIn Page.LogIn.Model
    | NotFound
    | ResetPassword Page.ResetPassword.Model
    | SignUp Page.SignUp.Model


main =
    Navigation.programWithFlags (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> Navigation.Location -> (Model, Cmd Msg)
init { token } location =
    let
        route = Route.parse location
    in
        ( Model Blank token
        , Task.perform RouteChange <| Task.succeed route
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg subModel subMsg ->
            let
                pageModel = Page.Home.update subMsg subModel
            in
                ( { model | page = Home pageModel }
                , Cmd.none
                )
    
        LoginMsg subModel subMsg ->
            let
                ( pageModel, subCmd, outCmd ) = Page.LogIn.update subMsg subModel
                ( newModel, cmd ) = 
                    case outCmd of
                        Page.LogIn.NoOp ->
                            ( model, Cmd.none )

                        Page.LogIn.SetToken token ->
                            ( { model | token = Just token }
                            , Cmd.batch 
                                [ Route.navigateTo <| Route.Protected Route.Home
                                , Interop.syncToken <| Just token
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
            ( { model | token = Nothing }
            , Cmd.batch
                [ Route.navigateTo <| Route.Public Route.LogIn
                , Interop.syncToken Nothing
                ]
            )
            
        ResetPasswordMsg subModel subMsg ->
            ( { model | page = ResetPassword <| Page.ResetPassword.update subMsg subModel }
            , Cmd.none
            )
    
        RouteChange route ->
            case route of
                Route.Protected page ->
                    case model.token of
                        Nothing ->
                            ( model, Route.navigateTo <| Route.Public Route.LogIn )

                        Just token ->
                            case page of
                                Route.Home ->
                                    let
                                        ( subModel, subCmd ) = Page.Home.init token
                                    in
                                        ( { model | page = Home subModel }
                                        , Cmd.map (HomeMsg subModel) subCmd
                                        )

                Route.Public page ->
                    case model.token of
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


frame : Html Msg -> Html Msg
frame pageView = 
    Html.div
        []
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
                        [ Html.text "Documents" ]
                    ]
                ]
            , Html.div 
                [ Attributes.class "navbar-end" ]
                [ Html.div
                    [ Attributes.class "navbar-item has-dropdown is-hoverable" ]
                    [ Html.a
                        [ Attributes.class "navbar-link" ]
                        [ Html.text "User" ]
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
    case model.page of
        Blank ->
            Html.div [] []
    
        Home subModel ->
            Page.Home.view subModel
                |> Html.map (HomeMsg subModel)
                |> frame
            
        LogIn subModel ->
            Page.LogIn.view subModel
                |> Html.map (LoginMsg subModel)  
            
        NotFound ->
            Page.NotFound.view
            
        ResetPassword subModel ->
            Page.ResetPassword.view
                |> Html.map (ResetPasswordMsg subModel)
            
        SignUp subModel ->
            Page.SignUp.view subModel
                |> Html.map (SignUpMsg subModel)