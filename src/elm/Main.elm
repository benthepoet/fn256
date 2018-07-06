module Main exposing (..)

import Elements
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
    = RouteChange Route.Route
    | SetToken String


type Page
    = Blank
    | Home
    | LogIn
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
                            ( { model | page = LogIn }
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
        elements =
            case model.page of
                Blank ->
                    []
            
                Home ->
                    [ Elements.column [] ]
                    
                LogIn ->
                    Page.LogIn.view
                    
                NotFound ->
                    Page.NotFound.view
                    
                ResetPassword ->
                    Page.ResetPassword.view
                    
                SignUp ->
                    Page.SignUp.view
    in
        Html.div 
            [ Attributes.class "columns" ] 
            elements