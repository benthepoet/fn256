module Main exposing (..)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Interop
import Navigation
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
    = LoginMsg Page.LogIn.Model Page.LogIn.Msg
    | ResetPasswordMsg Page.ResetPassword.Model Page.ResetPassword.Msg
    | RouteChange Route.Route
    | SetToken String
    | SignUpMsg Page.SignUp.Model Page.SignUp.Msg


type Page
    = Blank
    | Home
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


subscriptions model =
    Sub.none


update msg model =
    case msg of
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
                                    ( { model | page = Home }
                                    , Cmd.none
                                    )

                Route.Public page ->
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
                            
        SetToken token ->
            ( { model | token = Just token }
            , Cmd.none
            )
            
        SignUpMsg subModel subMsg ->
            ( { model | page = SignUp <| Page.SignUp.update subMsg subModel }
            , Cmd.none
            )

view model =
    let
        pageView =
            case model.page of
                Blank ->
                    Html.div [] []
            
                Home ->
                    Html.div [] []
                    
                LogIn subModel ->
                    Page.LogIn.view
                        |> Html.map (LoginMsg subModel)  
                    
                NotFound ->
                    Page.NotFound.view
                    
                ResetPassword subModel ->
                    Page.ResetPassword.view
                        |> Html.map (ResetPasswordMsg subModel)
                    
                SignUp subModel ->
                    Page.SignUp.view
                        |> Html.map (SignUpMsg subModel)
    in
        Html.div [] [ pageView ]