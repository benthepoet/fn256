module Page.Editor exposing (..)


import Array exposing (Array)
import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Elements
import Events.Svg
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Ports
import Request.Document
import Request.Element
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task
import View.Icons as Icons


type alias DragEvent =
    { index : Int
    , dx : Int
    , dy : Int
    }


type alias Model =
    { event : Maybe DragEvent
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
    | MouseClick
    | MouseMove Int Int
    | MouseUp Int Int
    | SetMode Mode


type Status
    = Saved
    | SyncFailure
    | Syncing


type ToolboxItem
    = Spacer
    | Tool (Html Msg) Mode


init id user = 
    let
        toolbox = 
            [ Tool Icons.pointer Select
            , Spacer
            , Tool Icons.barcode Barcode
            , Tool Icons.font Text
            , Tool Icons.square Shape
            ]
        model = Model Nothing Select Saved toolbox
        token = Just user.token
    in
        Task.map2 model 
            (Http.toTask <| Request.Document.get token id)
            (Task.map Array.fromList <| Http.toTask <| Request.Element.list token id)


getTarget { elements, event } =
    let
        getElement { index } =
            Array.get index elements
    in
        Maybe.andThen getElement event


moveElement : (Int, Int) -> DragEvent -> Element -> (Int, Element)
moveElement (x, y) { index, dx, dy } element =
    let 
        updateAttributes attributes =
            { attributes
            | x = x - dx + attributes.x
            , y = y - dy + attributes.y
            }

        elementType =
            case element.elementType of
                Element.Circle attributes ->
                    Element.Circle <| updateAttributes attributes
                        
                Element.Rect attributes ->
                    Element.Rect <| updateAttributes attributes
                    
                Element.TextBox attributes ->
                    Element.TextBox <| updateAttributes attributes
    in
        ( index
        , { element | elementType = elementType }
        )


update user msg model =
    let
        token = Maybe.map .token user
    in
        case (model.mode, msg) of
            (Shape, DocumentPosition (x, y)) ->
                let
                    size = 50
                    element = 
                        { elementType = Element.Rect 
                            { x = x - size // 2
                            , y = y - size // 2
                            , width = size
                            , height = size
                            }
                        }
                in
                    ( { model | status = Syncing }
                    , Http.send ElementCreated
                        <| Request.Element.create token model.document element
                    )
        
            (Shape, MouseUp x y) ->
                ( model
                , Ports.getDocumentPosition (x, y)
                )
        
            (Select, MouseDown index x y) ->
                ( { model | event = Just <| DragEvent index x y }
                , Cmd.none
                )
                
            (Select, MouseMove x y) ->
                case Maybe.map2 (moveElement (x, y)) model.event <| getTarget model of
                    Nothing ->
                        ( model, Cmd.none)
                        
                    Just (index, element) ->
                        ( { model 
                            | event = Just <| DragEvent index x y 
                            , elements = Array.set index element model.elements
                            }
                        , Cmd.none
                        )
                
            (Select, MouseUp _ _) ->
                case getTarget model of
                    Nothing ->
                        ( { model | event = Nothing }
                        , Cmd.none 
                        )
                        
                    Just element ->
                        ( { model 
                            | event = Nothing 
                            , status = Syncing
                            }
                        , Http.send ElementUpdated 
                            <| Request.Element.update token model.document element
                        )
                   
            (_, ElementCreated (Err _)) ->
                ( { model | status = SyncFailure }
                , Cmd.none 
                )
        
            (_, ElementCreated (Ok element)) ->
                ( { model 
                    | elements = Array.push element model.elements
                    , status = Saved 
                    }
                , Cmd.none
                )
        
            (_, ElementUpdated (Err _)) ->
                ( { model | status = SyncFailure }
                , Cmd.none 
                )
        
            (_, ElementUpdated (Ok element)) ->
                ( { model | status = Saved }
                , Cmd.none
                )
                        
            (_, MouseClick) ->
                ( Debug.log "model" model 
                , Cmd.none
                )
                        
            (_, SetMode mode) ->
                ( { model | mode = mode }
                , Cmd.none
                )
                
            (_, _) ->
                ( model, Cmd.none )


viewElement : Int -> Element -> Svg Msg
viewElement index element =
    let
        baseAttributes =
            (++)
                [ Svg.Attributes.class "cursor-pointer no-select"
                , Svg.Events.onClick MouseClick
                , Events.Svg.onMouseDown <| MouseDown index
                ]
    in
        case element.elementType of
            Element.Circle attributes ->
                Svg.circle
                    ( baseAttributes 
                        [ Svg.Attributes.cx <| toString attributes.x 
                        , Svg.Attributes.cy <| toString attributes.y 
                        , Svg.Attributes.r <| toString attributes.radius
                        ]
                    )
                    []
    
            Element.Rect attributes ->
                Svg.rect
                    ( baseAttributes
                        [ Svg.Attributes.x <| toString attributes.x 
                        , Svg.Attributes.y <| toString attributes.y 
                        , Svg.Attributes.width <| toString attributes.width
                        , Svg.Attributes.height <| toString attributes.height
                        ]
                    )
                    []
                    
            Element.TextBox attributes ->
                Svg.text_
                    ( baseAttributes
                        [ Svg.Attributes.x <| toString attributes.x
                        , Svg.Attributes.y <| toString attributes.y
                        ]
                    )
                    [ Svg.text attributes.text ]


viewProperties model =
    case getTarget model of
        Nothing ->
            Html.text "No element selected."
            
        Just element ->
            let
                baseFields attributes =
                    (++)
                        [ Elements.field
                            [ Elements.label [ Html.text "X" ] 
                            , Elements.text 
                                [ Attributes.value <| toString attributes.x ]
                            ]
                        , Elements.field
                            [ Elements.label [ Html.text "Y" ] 
                            , Elements.text 
                                [ Attributes.value <| toString attributes.y ]
                            ]
                        ]
            
                fields =
                    case element.elementType of
                        Element.Circle attributes ->
                            baseFields attributes 
                                [ Elements.field
                                    [ Elements.label [ Html.text "Radius" ]
                                    , Elements.text
                                        [ Attributes.value <| toString attributes.radius ]
                                    ]
                                ]
                        
                        Element.Rect attributes ->
                            baseFields attributes
                                [ Elements.field
                                    [ Elements.label [ Html.text "Width" ]
                                    , Elements.text
                                        [ Attributes.value <| toString attributes.width ]
                                    ]
                                , Elements.field
                                    [ Elements.label [ Html.text "Height" ]
                                    , Elements.text
                                        [ Attributes.value <| toString attributes.height ]
                                    ]
                                ]
                        
                        Element.TextBox attributes ->
                            baseFields attributes 
                                [ Elements.field
                                    [ Elements.label [ Html.text "Text" ]
                                    , Elements.text
                                        [ Attributes.value attributes.text ]
                                    ]
                                ]
            in
                Html.div [] fields


viewStatus : Status -> Html Msg
viewStatus status =
    let
        ( message, icon ) =
            case status of
                Saved ->
                    ("Saved", Icons.check)

                SyncFailure ->
                    ("Sync Failure", Icons.warning) 

                Syncing ->
                    ("Syncing", Icons.spinner)
    in
        Html.span
            [ Attributes.class "is-pulled-right" ]
            [ Html.text message
            , Html.span
                [ Attributes.class "icon pl-1 pr-1" ]
                [ icon ]
            ]
            
            
viewToolboxItem : Mode -> ToolboxItem -> Html Msg
viewToolboxItem mode item =
    case item of 
        Spacer ->
            Html.hr 
                [ Attributes.class "tool" ] 
                []
                
        Tool icon toolMode ->
            Html.span
                [ Attributes.classList
                    [ ("icon", True)
                    , ("tool", True)
                    , ("cursor-pointer", True)
                    , ("active", toolMode == mode)
                    ]
                , Events.onClick <| SetMode toolMode 
                ]
                [ icon ]


view : Model -> Html Msg
view model =
    let
        width = toString model.document.width
        height = toString model.document.height
        viewBox = String.join " " ["0", "0", width, height]
    in
        Html.div 
            [ Attributes.class "flex-1 flex-column" ]
            [ Html.div
                [ Attributes.class "p-05 shadow-b has-background-link has-text-white" ]
                [ Html.span
                    [ Attributes.class "icon pl-1 pr-1" ]
                    [ Icons.file ]
                , Html.span
                    [ Attributes.class "has-text-white has-text-semi-bold" ]
                    [ Html.text model.document.name ]
                , viewStatus model.status
                ]
            , Html.div
                [ Attributes.class "columns flex-1 mb-0 mt-0" ]
                [ Html.div
                    [ Attributes.class "column is-narrow pr-0 w-x-small shadow-r has-background-white" ]
                    [ Elements.columns 
                        [ Html.div
                            [ Attributes.class "column ml-0 mr-0 has-text-centered" ]
                            <| List.map (viewToolboxItem model.mode) model.toolbox
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
                            <| Array.indexedMap viewElement model.elements
                        ]
                    ]
                , Html.div
                    [ Attributes.class "column is-narrow pr-0 w-small shadow-l has-background-white" ]
                    [ Elements.columns 
                        [ Html.div
                            [ Attributes.class "p-1" ]
                            [ viewProperties model ] 
                        ]
                    ]
                ]
            ]