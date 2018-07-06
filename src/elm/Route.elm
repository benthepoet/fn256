module Route exposing (..)

import UrlParser exposing ((</>), int, map, oneOf, parseHash, s)


type ProtectedRoute
    = Home


type PublicRoute
    = LogIn
    | NotFound
    | ResetPassword
    | SignUp


type Route
    = Protected ProtectedRoute
    | Public PublicRoute


toPath route =
    case route of
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


parse location =
    if String.isEmpty location.hash then
        Protected Home
    else
        Maybe.withDefault (Public NotFound) (parseHash route location)


route =
    oneOf
        [ map (Protected Home) (s "")
        , map (Public LogIn) (s "login")
        , map (Public ResetPassword) (s "resetpassword")
        , map (Public SignUp) (s "signup")
        ]
