module Page.Editor exposing (..)


import Data.Document exposing (Document)
import Data.Element as Element exposing (Element)
import Elements
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Request.Document
import Request.Element
import Svg
import Svg.Attributes
import Svg.Events
import Task


type alias DragEvent =
    { target : Element
    , dx : Int
    , dy : Int
    }


type alias Model =
    { dragging : Maybe DragEvent
    , document : Document
    , elements : List Element
    }


type Msg
    = MouseDown Element Int Int
    | MouseMove Int Int
    | MouseUp


init id user = 
    let
        token = Just user.token
    in
        Task.map2 (Model Nothing) 
            (Http.toTask <| Request.Document.get token id)
            (Http.toTask <| Request.Element.list token id)

update user msg model =
    let
        token = Maybe.map .token user
    in
        case Debug.log "editor" msg of
            MouseDown target x y ->
                ( { model | dragging = Just <| DragEvent target x y }
                , Cmd.none
                )
                
            MouseMove x y ->
                case model.dragging of
                    Nothing ->
                        ( model, Cmd.none )
                    
                    Just { target, dx, dy } ->
                        case target of
                            Element.Circle attributes ->
                                let
                                    circle = Element.Circle { attributes 
                                                    | y = x - dx + attributes.y
                                                    , y = y - dy + attributes.y
                                                }
                                in
                                    ( { model | dragging = Just <| DragEvent circle x y }
                                    , Cmd.none
                                    )
                                    
                            Element.Rect attributes ->
                                let
                                    rect = Element.Rect { attributes
                                                | x = x - dx + attributes.x
                                                , y = y - dy + attributes.y
                                                }
                                in
                                    ( { model | dragging = Just <| DragEvent rect x y }
                                    , Cmd.none
                                    )
                
            MouseUp ->
                ( { model | dragging = Nothing }
                , Cmd.none
                )


viewCanvas =
    Svg.rect
        [ Svg.Attributes.x "0" 
        , Svg.Attributes.y "0" 
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.fill "#fff"
        ]
        []


viewElement element =
    case element of
        Element.Circle attributes ->
            Svg.circle
                [ Svg.Attributes.cx <| toString attributes.x 
                , Svg.Attributes.cy <| toString attributes.y 
                , Svg.Attributes.r <| toString attributes.radius
                , onMouseDown <| MouseDown element
                ]
                []

        Element.Rect attributes ->
            Svg.rect
                [ Svg.Attributes.x <| toString attributes.x 
                , Svg.Attributes.y <| toString attributes.y 
                , Svg.Attributes.width <| toString attributes.width
                , Svg.Attributes.height <| toString attributes.height
                , onMouseDown <| MouseDown element
                ]
                []

view { document, elements } =
    let
        width = toString document.width
        height = toString document.height
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
                            [ Svg.Attributes.class "shadow"
                            , Svg.Attributes.width width
                            , Svg.Attributes.height height
                            , Svg.Attributes.viewBox <| String.join " " ["0", "0", width, height]
                            , onMouseMove MouseMove
                            , Svg.Events.onMouseUp MouseUp
                            ]
                            (viewCanvas :: List.map viewElement elements)
                        ]
                    ]
                ]
            ]
    
mousePositionDecoder msg =
    Decode.map2 msg 
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)
        
        
onMouseDown msg =
    Svg.Events.on "mousedown" <| mousePositionDecoder msg
            
            
onMouseMove msg =
    Svg.Events.on "mousemove" <| mousePositionDecoder msg