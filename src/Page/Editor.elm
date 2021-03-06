module Page.Editor exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Browser.Dom
import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Elements
import Events.Html
import Events.Svg
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Page.Editor.Properties as Properties
import Request.Document
import Request.Element
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task
import View.Icons as Icons


type alias SelectEvent =
    { index : Int
    , dx : Int
    , dy : Int
    }


type alias Model =
    { event : Maybe SelectEvent
    , dragging : Bool
    , mode : Mode
    , status : Status
    , toolbox : List ToolboxItem
    , document : Document
    , elements : Array Element
    }


type Error
    = Dom Browser.Dom.Error
    | Http Http.Error


type Mode
    = Barcode
    | Select
    | Shape
    | Text


type Msg
    = ChangeProperty Property
    | ElementCreated (Result Error Element)
    | ElementUpdated (Result Http.Error Element)
    | MouseAction MouseMsg
    | SetMode Mode


type MouseMsg
    = MouseDown Int ( Int, Int )
    | MouseMove ( Int, Int )
    | MouseUp ( Int, Int )


type Property
    = Height Int
    | Radius Int
    | StrokeWidth Int
    | TextContent String
    | Width Int


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

        model =
            Model Nothing False Select Saved toolbox

        token =
            Just user.token
    in
    Task.map2 model
        (Http.toTask <| Request.Document.get token id)
        (Task.map Array.fromList <| Http.toTask <| Request.Element.list token id)


getTarget { elements, event } =
    let
        getElement { index } =
            case Array.get index elements of
                Nothing ->
                    Nothing

                Just element ->
                    Just ( index, element )
    in
    Maybe.andThen getElement event


moveElement : ( Int, Int ) -> SelectEvent -> ( Int, Element ) -> ( Int, Element )
moveElement ( x, y ) { index, dx, dy } ( _, element ) =
    ( index
    , { element
        | x = x - dx + element.x
        , y = y - dy + element.y
      }
    )


update user msg model =
    let
        token =
            Maybe.map .token user
    in
    case msg of
        ChangeProperty property ->
            let
                updateElement (index, element) =
                    let 
                        updated = 
                            case property of
                                Height value ->
                                    { element | height = value }
                                    
                                Radius value ->
                                    { element | radius = value }
                                    
                                StrokeWidth value ->
                                    { element | strokeWidth = value }
                                    
                                TextContent value ->
                                    { element | text = value }
                                    
                                Width value ->
                                    { element | width = value }
                    in
                    Array.set index updated model.elements
            in
            case Maybe.map updateElement <| getTarget model of
                Nothing ->
                    (model, Cmd.none)
                    
                Just elements ->
                    ( { model | elements = elements }
                    , Cmd.none
                    )
    
        MouseAction mouseMsg ->
            case ( model.mode, mouseMsg ) of
                ( Shape, MouseUp ( x, y ) ) ->
                    let
                        findElement =
                            Task.mapError Dom <| Browser.Dom.getElement "svg-document"

                        sendRequest { element } =
                            let
                                size =
                                    50

                                rect =
                                    Element.rect
                            in
                            Task.mapError Http <|
                                Http.toTask <|
                                    Request.Element.create token
                                        model.document
                                        { rect
                                            | x = x - round element.x - size // 2
                                            , y = y - round element.y - size // 2
                                            , width = size
                                            , height = size
                                            , strokeWidth = 1
                                        }
                    in
                    ( { model | status = Syncing }
                    , Task.attempt ElementCreated <|
                        Task.andThen sendRequest findElement
                    )

                ( Select, MouseDown index ( x, y ) ) ->
                    ( { model
                        | dragging = True
                        , event = Just <| SelectEvent index x y
                      }
                    , Cmd.none
                    )

                ( Select, MouseMove ( x, y ) ) ->
                    if model.dragging then
                        case Maybe.map2 (moveElement ( x, y )) model.event <| getTarget model of
                            Nothing ->
                                ( model, Cmd.none )

                            Just ( index, element ) ->
                                ( { model
                                    | event = Just <| SelectEvent index x y
                                    , elements = Array.set index element model.elements
                                  }
                                , Cmd.none
                                )

                    else
                        ( model, Cmd.none )

                ( Select, MouseUp _ ) ->
                    case getTarget model of
                        Nothing ->
                            ( { model | event = Nothing }
                            , Cmd.none
                            )

                        Just ( index, element ) ->
                            ( { model
                                | dragging = False
                                , status = Syncing
                              }
                            , Http.send ElementUpdated <|
                                Request.Element.update token model.document element
                            )

                ( _, _ ) ->
                    ( model, Cmd.none )

        ElementCreated (Err _) ->
            ( { model | status = SyncFailure }
            , Cmd.none
            )

        ElementCreated (Ok element) ->
            ( { model
                | elements = Array.push element model.elements
                , status = Saved
              }
            , Cmd.none
            )

        ElementUpdated (Err _) ->
            ( { model | status = SyncFailure }
            , Cmd.none
            )

        ElementUpdated (Ok element) ->
            ( { model | status = Saved }
            , Cmd.none
            )

        SetMode mode ->
            ( { model | mode = mode }
            , Cmd.none
            )


viewElement : Int -> Element -> Svg Msg
viewElement index element =
    let
        baseAttributes =
            (++)
                [ Svg.Attributes.class "cursor-pointer no-select"
                , Events.Svg.onMouseDown <| MouseAction << MouseDown index
                ]
    in
    case element.elementType of
        Element.Circle ->
            Svg.circle
                (baseAttributes
                    [ Svg.Attributes.cx <| String.fromInt element.x
                    , Svg.Attributes.cy <| String.fromInt element.y
                    , Svg.Attributes.r <| String.fromInt element.radius
                    , Svg.Attributes.fillOpacity "0"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth <| String.fromInt element.strokeWidth
                    ]
                )
                []

        Element.Rect ->
            Svg.rect
                (baseAttributes
                    [ Svg.Attributes.x <| String.fromInt element.x
                    , Svg.Attributes.y <| String.fromInt element.y
                    , Svg.Attributes.width <| String.fromInt element.width
                    , Svg.Attributes.height <| String.fromInt element.height
                    , Svg.Attributes.fillOpacity "0"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth <| String.fromInt element.strokeWidth
                    ]
                )
                []

        Element.TextBox ->
            Svg.text_
                (baseAttributes
                    [ Svg.Attributes.x <| String.fromInt element.x
                    , Svg.Attributes.y <| String.fromInt element.y
                    ]
                )
                [ Svg.text element.text ]


viewProperties model =
    case getTarget model of
        Nothing ->
            Html.text "No element selected."

        Just ( index, element ) ->
            let
                properties =
                    case element.elementType of
                        Element.Circle ->
                            [ Properties.x element
                            , Properties.y element
                            , Properties.radius element <| ChangeProperty << Radius
                            , Properties.strokeWidth element <| ChangeProperty << StrokeWidth
                            ]

                        Element.Rect ->
                            [ Properties.x element
                            , Properties.y element
                            , Properties.width element <| ChangeProperty << Width
                            , Properties.height element <| ChangeProperty << Height
                            , Properties.strokeWidth element <| ChangeProperty << StrokeWidth
                            ]

                        Element.TextBox ->
                            [ Properties.x element
                            , Properties.y element
                            , Properties.text element <| ChangeProperty << TextContent
                            ]
            in
            Html.div [] properties


viewStatus : Status -> Html Msg
viewStatus status =
    let
        ( message, icon ) =
            case status of
                Saved ->
                    ( "Saved", Icons.check )

                SyncFailure ->
                    ( "Sync Failure", Icons.warning )

                Syncing ->
                    ( "Syncing", Icons.spinner )
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
                    [ ( "icon", True )
                    , ( "tool", True )
                    , ( "cursor-pointer", True )
                    , ( "active", toolMode == mode )
                    ]
                , Events.onClick <| SetMode toolMode
                ]
                [ icon ]


view : Model -> Html Msg
view model =
    let
        width =
            String.fromInt model.document.width

        height =
            String.fromInt model.document.height

        viewBox =
            String.join " " [ "0", "0", width, height ]
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
                      <|
                        List.map (viewToolboxItem model.mode) model.toolbox
                    ]
                ]
            , Html.div
                [ Attributes.class "column has-text-centered overflow-y-scroll" ]
                [ Html.div
                    [ Attributes.class "mt-1" ]
                    [ Svg.svg
                        [ Svg.Attributes.id "svg-document"
                        , Svg.Attributes.class "shadow has-background-white"
                        , Svg.Attributes.width width
                        , Svg.Attributes.height height
                        , Svg.Attributes.viewBox viewBox
                        , Events.Svg.onMouseMove <| MouseAction << MouseMove
                        , Events.Svg.onMouseUp <| MouseAction << MouseUp
                        ]
                      <|
                        Array.toList <|
                            Array.indexedMap viewElement model.elements
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
