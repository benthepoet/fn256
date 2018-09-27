port module Ports exposing (syncUser)

import Json.Encode as Encode


port syncUser : Encode.Value -> Cmd msg
