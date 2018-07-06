import Html
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
    Html.div [] []