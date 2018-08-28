port module Ports exposing (documentPosition, getDocumentPosition, syncUser)

import Json.Encode as Encode


port getDocumentPosition : ( Int, Int ) -> Cmd msg


port syncUser : Encode.Value -> Cmd msg


port documentPosition : (( Int, Int ) -> msg) -> Sub msg
