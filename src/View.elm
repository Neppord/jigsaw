module View exposing (view)

import Array as A
import Dict
import Edge exposing (EdgePoints)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import JigsawImage
    exposing
        ( JigsawImage
        , PieceGroup
        , pieceIdToOffset
        )
import Model
    exposing
        ( Box
        , Model
        , Msg(..)
        , SelectionBox(..)
        , boxBottomRight
        , boxTopLeft
        )
import Point exposing (Point)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy
import SvgUtil


view : Model -> Html Msg
view model =
    Html.div
        turnOffTheBloodyImageDragging
        [ Html.button
            [ Html.Events.onClick Scramble ]
            [ Html.text "scramble" ]
        , Html.div
            [ Html.Attributes.style "width" <| String.fromInt model.image.width ++ "px"
            , Html.Attributes.style "height" <| String.fromInt model.image.height ++ "px"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "100px"
            , Html.Attributes.style "left" "0px"
            ]
            (viewSelectionBox (model.maxZLevel + 1) model.selectionBox ++ viewDiv model)
        ]


turnOffTheBloodyImageDragging : List (Attribute msg)
turnOffTheBloodyImageDragging =
    [ Html.Attributes.style "-webkit-user-select" "none"
    , Html.Attributes.style "-khtml-user-select" "none"
    , Html.Attributes.style "-moz-user-select" "none"
    , Html.Attributes.style "-o-user-select" "none"
    , Html.Attributes.style "user-select" "none"
    , Html.Attributes.draggable "false"
    ]


lazyDivSelectionBox : Int -> Box -> String -> Svg.Svg msg
lazyDivSelectionBox =
    Svg.Lazy.lazy3 divSelectionBox


divSelectionBox : Int -> Box -> String -> Svg.Svg msg
divSelectionBox zIndex box color =
    let
        topLeft =
            boxTopLeft box

        bottomRight =
            boxBottomRight box

        top =
            max 100 topLeft.y
    in
    Html.div
        ([ Html.Attributes.style "width" <| String.fromInt (bottomRight.x - topLeft.x) ++ "px"
         , Html.Attributes.style "height" <| String.fromInt (bottomRight.y - top) ++ "px"
         , Html.Attributes.style "background-color" color
         , Html.Attributes.style "border-style" "dotted"
         , Html.Attributes.style "top" <| String.fromInt (top - 100) ++ "px"
         , Html.Attributes.style "left" <| String.fromInt topLeft.x ++ "px"
         , Html.Attributes.style "z-index" <| String.fromInt zIndex
         , Html.Attributes.style "position" "absolute"
         ]
            ++ turnOffTheBloodyImageDragging
        )
        []


viewSelectionBox : Int -> SelectionBox -> List (Html msg)
viewSelectionBox zIndex selectionBox =
    case selectionBox of
        Normal box ->
            [ lazyDivSelectionBox zIndex box "rgba(0,0,255,0.2)" ]

        Inverted box ->
            [ lazyDivSelectionBox zIndex box "rgba(0,255,0,0.2)" ]

        NullBox ->
            []


lazyPieceDiv : JigsawImage -> PieceGroup -> Int -> Html msg
lazyPieceDiv =
    Html.Lazy.lazy3 pieceDiv


pieceDiv : JigsawImage -> PieceGroup -> Int -> Html msg
pieceDiv image pg pid =
    let
        offset =
            pieceIdToOffset image pid

        w =
            floor <| image.scale * toFloat (2 * image.width // image.xpieces)

        h =
            floor <| image.scale * toFloat (2 * image.height // image.ypieces)

        top =
            String.fromInt (pg.position.y + offset.y - h // 4) ++ "px"

        left =
            String.fromInt (pg.position.x + offset.x - w // 4) ++ "px"

        color =
            if pg.isSelected then
                "red"

            else
                "black"
    in
    Html.div
        [ Html.Attributes.style "z-index" <| String.fromInt pg.zlevel
        , Html.Attributes.style "filter" <| "drop-shadow(0px 0px 2px " ++ color ++ ")"
        , Html.Attributes.style "position" "absolute"
        ]
        [ Html.div
            ([ Html.Attributes.style "width" <| String.fromInt w ++ "px"
             , Html.Attributes.style "height" <| String.fromInt h ++ "px"
             , Html.Attributes.style "position" "absolute"
             , Html.Attributes.style "top" top
             , Html.Attributes.style "left" left
             , Html.Attributes.style "z-index" <| String.fromInt pg.zlevel
             , Html.Attributes.style "clipPath" <| clipPathRef pid
             , Html.Attributes.style "background-image" <| "url('" ++ image.path ++ "')"
             , Html.Attributes.style "background-size" <|
                String.fromInt (floor <| image.scale * toFloat image.width)
                    ++ "px "
                    ++ String.fromInt (floor <| image.scale * toFloat image.height)
                    ++ "px"
             , Html.Attributes.style "background-position" <|
                String.fromInt (w // 4 - offset.x)
                    ++ "px "
                    ++ String.fromInt (h // 4 - offset.y)
                    ++ "px"
             ]
                ++ turnOffTheBloodyImageDragging
            )
            []
        ]


viewDiv : Model -> List (Html Msg)
viewDiv model =
    let
        pieceGroupDiv : PieceGroup -> List ( String, Html msg )
        pieceGroupDiv pg =
            let
                render pid =
                    ( "piece-" ++ String.fromInt pid
                    , lazyPieceDiv model.image pg pid
                    )
            in
            List.map render pg.members

        viewPieces =
            model.pieceGroups
                |> Dict.values
                |> List.filter
                    (\pieceGroup -> Set.member pieceGroup.visibilityGroup model.visibleGroups)
                |> List.map pieceGroupDiv
                |> List.concat

        clipPathDefs =
            lazyclipPathDefs model.image model.edgePoints
    in
    [ Html.Keyed.node
        "div"
        []
        viewPieces
    , Svg.svg
        []
        [ clipPathDefs ]
    ]


lazyclipPathDefs : JigsawImage -> A.Array EdgePoints -> Svg.Svg msg
lazyclipPathDefs =
    Svg.Lazy.lazy2 (\image points -> Svg.defs [] (definePieceClipPaths image points))


definePieceClipPaths : JigsawImage -> A.Array EdgePoints -> List (Svg msg)
definePieceClipPaths image edgePoints =
    List.map (piecePath image edgePoints) (List.range 0 (image.xpieces * image.ypieces - 1))


piecePath : JigsawImage -> A.Array EdgePoints -> Int -> Svg msg
piecePath image edgePoints id =
    let
        w =
            image.scale * toFloat (image.width // image.xpieces)

        h =
            image.scale * toFloat (image.height // image.ypieces)

        offset =
            Point (floor (w / 2)) (floor (h / 2))

        curve =
            SvgUtil.pieceCurveFromPieceId image.xpieces image.ypieces id edgePoints

        move =
            "translate(" ++ Point.toString offset ++ ") "

        scale =
            "scale(" ++ String.fromFloat (w / 200.0) ++ " " ++ String.fromFloat (h / 200.0) ++ ")"
    in
    Svg.clipPath
        [ Svg.Attributes.id <| pieceClipId id ]
        [ Svg.path
            [ Svg.Attributes.id <| pieceOutlineId id
            , Svg.Attributes.transform <| move ++ scale
            , Svg.Attributes.d curve
            , Svg.Attributes.fillOpacity "0.0"
            ]
            []
        ]


pieceOutlineId : Int -> String
pieceOutlineId id =
    "piece-" ++ String.fromInt id ++ "-outline"


pieceClipId : Int -> String
pieceClipId id =
    "piece-" ++ String.fromInt id ++ "-clip"


clipPathRef : Int -> String
clipPathRef id =
    "url(#" ++ pieceClipId id ++ ")"
