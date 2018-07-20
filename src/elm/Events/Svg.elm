module Events.Svg exposing (..)


import Json.Decode as Decode
import Svg.Events


mousePositionEvent name msg =
    let
        decoder =
            Decode.map2 msg 
                (Decode.field "clientX" Decode.int)
                (Decode.field "clientY" Decode.int)
    in
        Svg.Events.on name decoder


onMouseDown =
    mousePositionEvent "mousedown"

    
onMouseMove =
    mousePositionEvent "mousemove"
