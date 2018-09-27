module Route exposing (ProtectedRoute(..), PublicRoute(..), Route(..), href, navigateTo, parse, route, toPath)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Url.Parser as Parser exposing ((</>), int, map, oneOf, s, top)


type ProtectedRoute
    = Editor Int
    | Home


type PublicRoute
    = LogIn
    | NotFound
    | ResetPassword
    | SignUp


type Route
    = Protected ProtectedRoute
    | Public PublicRoute


toPath : Route -> String
toPath from =
    case from of
        Protected (Editor id) ->
            "/editor/" ++ String.fromInt id

        Protected Home ->
            "/"

        Public LogIn ->
            "/login"

        Public NotFound ->
            "/notfound"

        Public ResetPassword ->
            "/resetpassword"

        Public SignUp ->
            "/signup"


href : Route -> Html.Attribute msg
href =
    Attributes.href << toPath


navigateTo : Navigation.Key -> Route -> Cmd msg
navigateTo key =
    Navigation.replaceUrl key << toPath


parse url =
    if String.isEmpty url.path then
        Protected Home

    else
        Maybe.withDefault (Public NotFound) (Parser.parse route url)


route =
    oneOf
        [ map (Protected << Editor) (s "editor" </> int)
        , map (Protected Home) top
        , map (Public LogIn) (s "login")
        , map (Public ResetPassword) (s "resetpassword")
        , map (Public SignUp) (s "signup")
        ]
