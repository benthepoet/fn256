module Main exposing (..)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Navigation
import Page.LogIn
import Page.NotFound
import Page.ResetPassword
import Page.SignUp
import Route
import Task


type alias Model =
    { page : Page
    , token : Maybe String
    }


type Msg 
    = LoginMsg Page.LogIn.Model Page.LogIn.Msg
    | RouteChange Route.Route
    | SetToken String


type Page
    = Blank
    | Home
    | LogIn Page.LogIn.Model
    | NotFound
    | ResetPassword
    | SignUp


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
        ( Model Blank Nothing
        , Task.perform RouteChange <| Task.succeed route
        )


subscriptions model =
    Sub.none


update msg model =
    case msg of
        LoginMsg subModel subMsg ->
            ( { model | page = LogIn <| Page.LogIn.update subMsg subModel }
            , Cmd.none
            )
    
        RouteChange route ->
            case route of
                Route.Protected page ->
                    case model.token of
                        Nothing ->
                            ( model, Navigation.modifyUrl (Route.toPath <| Route.Public Route.LogIn) )

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
                            ( { model | page = ResetPassword }
                            , Cmd.none
                            )
                            
                        Route.SignUp ->
                            ( { model | page = SignUp }
                            , Cmd.none
                            )
                            
        SetToken token ->
            ( { model | token = Just token }
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
                    
                ResetPassword ->
                    Page.ResetPassword.view
                    
                SignUp ->
                    Page.SignUp.view
    in
        Html.div [] [ pageView ]