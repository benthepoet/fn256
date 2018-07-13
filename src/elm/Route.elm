module Route exposing (..)


import Html
import Html.Attributes as Attributes
import Navigation
import UrlParser exposing ((</>), int, map, oneOf, parseHash, s)


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
toPath route =
    case route of
        Protected (Editor id) ->
            "#/editor/" ++ (toString id)
    
        Protected Home ->
            "#/"

        Public LogIn ->
            "#/login"

        Public NotFound ->
            "#/notfound"

        Public ResetPassword ->
            "#/resetpassword"

        Public SignUp ->
            "#/signup"


href : Route -> Html.Attribute msg
href =
    Attributes.href << toPath


navigateTo : Route -> Cmd msg
navigateTo =
    Navigation.modifyUrl << toPath


parse location =
    if String.isEmpty location.hash then
        Protected Home
    else
        Maybe.withDefault (Public NotFound) (parseHash route location)


route =
    oneOf
        [ map (Protected << Editor) (s "editor" </> int)
        , map (Protected Home) (s "")
        , map (Public LogIn) (s "login")
        , map (Public ResetPassword) (s "resetpassword")
        , map (Public SignUp) (s "signup")
        ]
