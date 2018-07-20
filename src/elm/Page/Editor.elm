module Page.Editor exposing (..)


import Array exposing (Array)
import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Elements
import Events.Svg
import Html
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import Request.Document
import Request.Element
import Svg
import Svg.Attributes
import Svg.Events
import Task


type alias DragEvent =
    { index : Int
    , dx : Int
    , dy : Int
    }


type alias Model =
    { dragging : Maybe DragEvent
    , status : Status
    , document : Document
    , elements : Array Element
    }


type Msg
    = ElementUpdated (Result Http.Error Element)
    | MouseDown Int Int Int
    | MouseMove Int Int
    | MouseUp


type Status
    = Error
    | Saved
    | Syncing


init id user = 
    let
        token = Just user.token
    in
        Task.map2 (Model Nothing Saved) 
            (Http.toTask <| Request.Document.get token id)
            (Task.map Array.fromList <| Http.toTask <| Request.Element.list token id)

update user msg model =
    let
        getElement = model.elements |> flip Array.get
        getTarget = Maybe.andThen <| getElement << .index
        token = Maybe.map .token user
    in
        case msg of
            ElementUpdated (Err _) ->
                ( { model | status = Error }
                , Cmd.none 
                )
        
            ElementUpdated (Ok element) ->
                ( { model | status = Saved }
                , Cmd.none
                )
        
            MouseDown index x y ->
                ( { model | dragging = Just <| DragEvent index x y }
                , Cmd.none
                )
                
            MouseMove x y ->
                let
                    updatePosition { index, dx, dy } element =
                        case element of
                            Element.Circle attributes ->
                                ( index
                                , Element.Circle 
                                    { attributes 
                                    | x = x - dx + attributes.x
                                    , y = y - dy + attributes.y
                                    }
                                )
                                    
                            Element.Rect attributes ->
                                ( index
                                , Element.Rect 
                                    { attributes
                                    | x = x - dx + attributes.x
                                    , y = y - dy + attributes.y
                                    }
                                )
                in
                    case Maybe.map2 updatePosition model.dragging <| getTarget model.dragging of
                        Nothing ->
                            ( model, Cmd.none)
                            
                        Just (index, element) ->
                            ( { model 
                                | dragging = Just <| DragEvent index x y 
                                , elements = Array.set index element model.elements
                                }
                            , Cmd.none
                            )
                
            MouseUp ->
                case getTarget model.dragging of
                    Nothing ->
                        ( { model | dragging = Nothing }
                        , Cmd.none 
                        )
                        
                    Just element ->
                        ( { model 
                            | dragging = Nothing 
                            , status = Syncing
                            }
                        , Http.send ElementUpdated 
                            <| Request.Element.update token model.document element
                        )


viewElement index element =
    let
        sharedAttributes =
            [ Svg.Attributes.class "cursor-pointer"
            , Events.Svg.onMouseDown <| MouseDown index
            ]
    in
        case element of
            Element.Circle attributes ->
                Svg.circle
                    ( sharedAttributes ++ 
                        [ Svg.Attributes.cx <| toString attributes.x 
                        , Svg.Attributes.cy <| toString attributes.y 
                        , Svg.Attributes.r <| toString attributes.radius
                        ]
                    )
                    []
    
            Element.Rect attributes ->
                Svg.rect
                    ( sharedAttributes ++
                        [ Svg.Attributes.x <| toString attributes.x 
                        , Svg.Attributes.y <| toString attributes.y 
                        , Svg.Attributes.width <| toString attributes.width
                        , Svg.Attributes.height <| toString attributes.height
                        ]
                    )
                    []

view { document, elements, status } =
    let
        width = toString document.width
        height = toString document.height
        ( statusIcon, statusMessage ) =
            case status of
                Error ->
                    ("exclamation-triangle", "Error") 
            
                Saved ->
                    ("check-circle", "Saved")

                Syncing ->
                    ("spinner fa-pulse", "Syncing")
    in
        Html.div 
            [ Attributes.class "flex-1 flex-column" ]
            [ Html.div
                [ Attributes.class "pl shadow-b has-background-link has-text-white" ]
                [ Html.span
                    [ Attributes.class "icon pl-1 pr-1" ]
                    [ Elements.fas "file-alt" ]
                , Html.span
                    [ Attributes.class "has-text-white has-text-semi-bold" ]
                    [ Html.text document.name ]
                , Html.span
                    [ Attributes.class "is-pulled-right" ]
                    [ Html.text statusMessage
                    , Html.span
                        [ Attributes.class "icon pl-1 pr-1" ]
                        [ Elements.fas statusIcon ]
                    ]
                ]
            , Html.div
                [ Attributes.class "columns flex-1 mb-0 mt-0" ]
                [ Html.div
                    [ Attributes.class "column is-narrow pr-0 w-x-small shadow-r has-background-white" ]
                    [ Elements.columns 
                        [ Html.div
                            [ Attributes.class "column ml-0 mr-0 has-text-centered" ]
                            [ Html.span
                                [ Attributes.class "icon tool" ]
                                [ Elements.fas "mouse-pointer" ]
                            , Html.hr 
                                [ Attributes.class "tool" ] 
                                []
                            , Html.span
                                [ Attributes.class "icon tool" ]
                                [ Elements.fas "barcode" ]
                            , Html.span
                                [ Attributes.class "icon tool" ]
                                [ Elements.fas "font" ]
                            , Html.span
                                [ Attributes.class "icon tool" ]
                                [ Elements.far "square" ]
                            ]
                        ]
                    ]
                , Html.div
                    [ Attributes.class "column has-text-centered overflow-y-scroll" ]
                    [ Html.div 
                        [ Attributes.class "mt-1" ]
                        [ Svg.svg 
                            [ Svg.Attributes.class "shadow has-background-white"
                            , Svg.Attributes.width width
                            , Svg.Attributes.height height
                            , Svg.Attributes.viewBox <| String.join " " ["0", "0", width, height]
                            , Events.Svg.onMouseMove MouseMove
                            , Svg.Events.onMouseUp MouseUp
                            ]
                            <| Array.toList 
                            <| Array.indexedMap viewElement elements
                        ]
                    ]
                ]
            ]