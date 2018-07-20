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
    = Saved
    | SyncFailure
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
                ( { model | status = SyncFailure }
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
                        ( index
                        , { element 
                            | x = x - dx + element.x
                            , y = y - dy + element.y
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
        x = toString element.x
        y = toString element.y
        sharedAttributes =
            [ Svg.Attributes.class "cursor-pointer"
            , Events.Svg.onMouseDown <| MouseDown index
            ]
    in
        case element.elementType of
            Element.Circle ->
                Svg.circle
                    ( sharedAttributes ++ 
                        [ Svg.Attributes.cx x 
                        , Svg.Attributes.cy y 
                        , Svg.Attributes.r <| toString element.radius
                        ]
                    )
                    []
    
            Element.Rect ->
                Svg.rect
                    ( sharedAttributes ++
                        [ Svg.Attributes.x x 
                        , Svg.Attributes.y y 
                        , Svg.Attributes.width <| toString element.width
                        , Svg.Attributes.height <| toString element.height
                        ]
                    )
                    []


viewStatus status =
    let
        ( message, icon ) =
            case status of
                Saved ->
                    ("Saved", "check-circle")

                SyncFailure ->
                    ("Sync Failure", "exclamation-triangle") 

                Syncing ->
                    ("Syncing", "spinner fa-pulse")
    in
        Html.span
            [ Attributes.class "is-pulled-right" ]
            [ Html.text message
            , Html.span
                [ Attributes.class "icon pl-1 pr-1" ]
                [ Elements.fas icon ]
            ]


view { document, elements, status } =
    let
        width = toString document.width
        height = toString document.height
        viewBox = String.join " " ["0", "0", width, height]
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
                , viewStatus status
                ]
            , Html.div
                [ Attributes.class "columns flex-1 mb-0 mt-0" ]
                [ Html.div
                    [ Attributes.class "column is-narrow pr-0 w-x-small shadow-r has-background-white" ]
                    [ Elements.columns 
                        [ Html.div
                            [ Attributes.class "column ml-0 mr-0 has-text-centered" ]
                            [ Html.span
                                [ Attributes.class "icon tool cursor-pointer" ]
                                [ Elements.fas "mouse-pointer" ]
                            , Html.hr 
                                [ Attributes.class "tool" ] 
                                []
                            , Html.span
                                [ Attributes.class "icon tool cursor-pointer" ]
                                [ Elements.fas "barcode" ]
                            , Html.span
                                [ Attributes.class "icon tool cursor-pointer" ]
                                [ Elements.fas "font" ]
                            , Html.span
                                [ Attributes.class "icon tool cursor-pointer" ]
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
                            , Svg.Attributes.viewBox viewBox
                            , Events.Svg.onMouseMove MouseMove
                            , Svg.Events.onMouseUp MouseUp
                            ]
                            <| Array.toList 
                            <| Array.indexedMap viewElement elements
                        ]
                    ]
                ]
            ]