module Page.Home exposing (..)


import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request


type alias Model =
    { documents : List Request.Document
    , isError : Bool
    , isLoading : Bool
    }


type Msg
    = LoadDocuments (Result Http.Error (List Request.Document))


init token = 
    ( Model [] False True
    , Http.send LoadDocuments <| Request.getDocuments <| Just token
    )
    
    
update msg model =
    case msg of
        LoadDocuments (Ok documents) ->
            { model
            | documents = documents
            , isError = False
            , isLoading = False
            }
            
        LoadDocuments (Err _) ->
            { model
            | isError = True
            , isLoading = False
            }


view subModel =
    Elements.columns
        [ Elements.column [] ]