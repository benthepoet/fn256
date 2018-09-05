module Events.Svg exposing (mousePositionEvent, onMouseDown, onMouseMove, onMouseUp)

import Json.Decode as Decode
import Svg.Events


mousePositionEvent name x y msg =
    let
        decoder =
            Decode.map msg <|
                Decode.map2 (\a b -> (a, b))
                    (Decode.field x Decode.int)
                    (Decode.field y Decode.int)
    in
    Svg.Events.on name decoder


onMouseDown =
    mousePositionEvent "mousedown" "clientX" "clientY"


onMouseMove =
    mousePositionEvent "mousemove" "clientX" "clientY"


onMouseUp =
    mousePositionEvent "mouseup" "clientX" "clientY"
