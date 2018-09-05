module Events.Html exposing (onInputInt)


import Html.Events as Events
import Json.Decode as Decode


decodeStringToInt str =
    case String.toInt str of
        Nothing ->
            Decode.fail "The string is not a valid integer."
            
        Just value ->
            Decode.succeed value


onInputInt msg = 
    Events.stopPropagationOn "input"
        <| Decode.map (\value -> (msg value, True))
        <| Decode.andThen decodeStringToInt Events.targetValue