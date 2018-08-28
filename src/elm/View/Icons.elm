module View.Icons exposing (barcode, check, far, fas, file, font, lock, mail, pointer, root, search, spinner, square, warning)

import Html
import Html.Attributes as Attributes


root classes =
    Html.i
        [ Attributes.class <| String.join " " classes
        ]
        []


far =
    root << (::) "far"


fas =
    root << (::) "fas"


barcode =
    fas [ "fa-barcode" ]


check =
    fas [ "fa-check-circle" ]


file =
    fas [ "fa-file-alt" ]


font =
    fas [ "fa-font" ]


lock =
    fas [ "fa-lock" ]


mail =
    fas [ "fa-envelope" ]


pointer =
    fas [ "fa-mouse-pointer" ]


search =
    fas [ "fa-search" ]


spinner =
    fas [ "fa-spinner", "fa-pulse" ]


square =
    far [ "fa-square" ]


warning =
    fas [ "fa-exclamation-triangle" ]
