port module Ports exposing (..)


import Json.Encode as Encode


port syncUser : Encode.Value -> Cmd msg