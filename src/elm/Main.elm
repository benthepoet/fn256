import Html


type alias Model =
    { token : Maybe String
    }


type Msg = 
    Login String


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

        
init =
    (Model Nothing, Cmd.none)


subscriptions model =
    Sub.none


update msg model =
    case msg of
        Login token ->
            (Model <| Just token, Cmd.none)


view model =
    Html.div [] []