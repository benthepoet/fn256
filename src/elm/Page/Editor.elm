module Page.Editor exposing (..)


import Array exposing (Array)
import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Elements
import Events.Svg
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Ports
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
    , mode : Mode
    , status : Status
    , toolbox : List ToolboxItem
    , document : Document
    , elements : Array Element
    }


type Mode
    = Barcode
    | Select
    | Shape
    | Text


type Msg
    = DocumentPosition (Int, Int)
    | ElementCreated (Result Http.Error Element)
    | ElementUpdated (Result Http.Error Element)
    | MouseDown Int Int Int
    | MouseMove Int Int
    | MouseUp Int Int
    | SetMode Mode


type Status
    = Saved
    | SyncFailure
    | Syncing


type ToolboxItem
    = Spacer
    | Tool String Mode


init id user = 
    let
        toolbox = 
            [ Tool "mouse-pointer" Select
            , Spacer
            , Tool "barcode" Barcode
            , Tool "font" Text
            , Tool "square" Shape
            ]
        model = Model Nothing Select Saved toolbox
        token = Just user.token
    in
        Task.map2 model 
            (Http.toTask <| Request.Document.get token id)
            (Task.map Array.fromList <| Http.toTask <| Request.Element.list token id)

update user msg model =
    let
        getElement = model.elements |> flip Array.get
        getTarget = Maybe.andThen <| getElement << .index
        token = Maybe.map .token user
    in
        case ( msg, model.mode ) of
            (DocumentPosition (x, y), Shape) ->
                let
                    element = 
                        { x = x
                        , y = y
                        , elementType = Element.Rect
                        , width = 50
                        , height = 50
                        , radius = 0
                        }
                in
                    ( { model | status = Syncing }
                    , Http.send ElementCreated
                        <| Request.Element.create token model.document element
                    )
            
            (ElementCreated (Err _), _) ->
                ( { model | status = SyncFailure }
                , Cmd.none 
                )
        
            (ElementCreated (Ok element), _) ->
                ( { model 
                    | elements = Array.push element model.elements
                    , status = Saved 
                    }
                , Cmd.none
                )
        
            (ElementUpdated (Err _), _) ->
                ( { model | status = SyncFailure }
                , Cmd.none 
                )
        
            (ElementUpdated (Ok element), _) ->
                ( { model | status = Saved }
                , Cmd.none
                )
        
            (MouseDown index x y, Select) ->
                ( { model | dragging = Just <| DragEvent index x y }
                , Cmd.none
                )
                
            (MouseMove x y, Select) ->
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
                
            (MouseUp _ _, Select) ->
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
                        
            (MouseUp x y, Shape) ->
                ( model
                , Ports.getDocumentPosition (x, y)
                )
                        
            (SetMode mode, _) ->
                ( { model | mode = mode }
                , Cmd.none
                )
                
            (_, _) ->
                ( model, Cmd.none )


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
            
            
viewToolboxItem mode item =
    case item of 
        Spacer ->
            Html.hr 
                [ Attributes.class "tool" ] 
                []
                
        Tool icon toolMode ->
            let
                activeClass = 
                    if toolMode == mode then
                        "active"
                    else
                        "inactive"
            in
                Html.span
                    [ Attributes.class <| "icon tool cursor-pointer " ++ activeClass
                    , Events.onClick <| SetMode toolMode 
                    ]
                    [ Elements.fas icon ]


view { document, elements, mode, status, toolbox } =
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
                            <| List.map (viewToolboxItem mode) toolbox
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
                            , Events.Svg.onMouseUp MouseUp
                            ]
                            <| Array.toList 
                            <| Array.indexedMap viewElement elements
                        ]
                    ]
                ]
            ]