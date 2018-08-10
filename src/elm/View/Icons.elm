module View.Icons exposing (..)


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
    fas ["fa-barcode"]


check =
    fas ["fa-check-circle"]


file =
    fas ["fa-file-alt"]
    
    
font =
    fas ["fa-font"]
    
    
pointer =
    fas ["fa-mouse-pointer"]
    

spinner =
    fas ["fa-spinner", "fa-pulse"]


square =
    far ["fa-square"]


warning =
    fas ["fa-exclamation-triangle"]