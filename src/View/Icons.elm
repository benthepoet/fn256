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
    root [ "icon-barcode" ]


check =
    root [ "icon-checkbox-checked" ]


file =
    root [ "icon-file-text" ]


font =
    root [ "icon-font1" ]


lock =
    root [ "icon-key" ]


mail =
    root [ "icon-envelop" ]


pointer =
    root [ "icon-mouse-pointer" ]


search =
    root [ "icon-search" ]


spinner =
    root [ "icon-spinner2" ]


square =
    root [ "icon-checkbox-unchecked" ]


warning =
    root [ "icon-exclamation-triangle" ]
